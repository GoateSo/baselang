package baselang;

import fastparse.*, SingleLineWhitespace.*

inline def index[$: P, T](inline p: P[T]): P[(Int, T)] = P(Index ~ p)

def id[$: P]: P[Location.Var] =
  import NoWhitespace.*
  index(ident).map(Location.Var.apply)

def program[$: P]: P[Seq[TopLevel]] =
  import MultiLineWhitespace.*
  Start ~ decl.rep ~ End

def lineCol(index: Int, input: String): (Int, Int) =
  val relevant = input.take(index)
  val lines = relevant.count(_ == '\n') + 1
  val lastNL = relevant.lastIndexOf('\n')
  val col = index - lastNL
  (lines, col)

// top level declarations
def decl[$: P]: P[TopLevel] =
  import TopLevel.*
  def param[$: P]: P[VarDecl.VDecl] = (typepar ~ id).map(VarDecl.VDecl.apply)
  def fundecl[$: P]: P[TopLevel] =
    import MultiLineWhitespace.*, Location.Var
    (typepar ~/ id ~/ "{" ~ param.rep(sep = ",") ~ "}" ~ block)
      .map(FunDecl.apply)
  def tupdecl[$: P]: P[TopLevel] =
    import MultiLineWhitespace.*
    ("tuple" ~ id ~ "{" ~ vdecl.rep ~ "}" ~ ".").map(TupDecl.apply)

  P((vdecl.map(VDecl.apply) | fundecl | tupdecl) ~ com.?)

// statements and blocks
def com[$: P]: P[Unit] =
  import NoWhitespace.*
  P(("$" | "!!") ~ CharsWhile(_ != '\n').rep ~ "\n")

def ifstmt[$: P]: P[Stmt] =
  import Stmt.*, MultiLineWhitespace.*
  ("if" ~ expr ~ block ~ ("else" ~ block).?).map(If.apply)

def stmt[$: P]: P[Stmt] =
  import Stmt.*
  P(
    ifstmt
      | ("while" ~/ expr ~/ block).map(While.apply)
      | ("return" ~ expr.? ~ ".").map(Return.apply)
      | ("read" ~/ ">>" ~ loc ~ ".").map(Read.apply)
      | ("write" ~/ "<<" ~ expr ~ ".").map(Write.apply)
      | (loc ~ "++" ~ ".").map(Incr.apply)
      | (loc ~ "--" ~ ".").map(Decr.apply)
      | ((assign | call) ~ ".").map(ExprStmt.apply)
  ) ~ com.?

def vdecl[$: P]: P[VarDecl] =
  import MultiLineWhitespace.*, VarDecl.*
  def tuple = ("tuple" ~ id ~ id ~ "." ~ com.?) map (TupVDecl.apply)
  def variable = (typepar ~ id ~ "." ~ com.?) map (VDecl.apply)
  P(tuple | variable)

private inline def list[$: P, T](inline p: P[T]): P[Seq[T]] =
  import MultiLineWhitespace.*
  (p | com).rep.map(_.flatMap {
    case x: T => Seq(x)
    case _    => Nil
  })

def block[$: P]: P[Body] =
  import MultiLineWhitespace.*
  P("[" ~ list(vdecl) ~ list(stmt) ~ "]" ~ com.?)

// expressions
import Expr.*
def expr[$: P]: P[Expr] = assign | or
def assign[$: P]: P[Expr] =
  (loc ~ "=" ~ expr) map ((l, r) => Assign(l.ind, l, r))
def or[$: P]: P[Expr] = binop(and, CharIn("|").!, and)
def and[$: P]: P[Expr] = binop(compare, CharIn("&").!, compare)
def compare[$: P]: P[Expr] =
  binop(addsub, StringIn("==", "~=", "<", ">", "<=", ">=").!, addsub)
def addsub[$: P]: P[Expr] = binop(muldiv, CharIn("+\\-").!, muldiv)
def muldiv[$: P]: P[Expr] = binop(unary, CharIn("*/").!, unary)

def unary[$: P]: P[Expr] =
  "-" ~ unary.map(e => UnOp(e.ind, UnaryOp.Neg, e))
    | "~" ~ unary.map(e => UnOp(e.ind, UnaryOp.Not, e))
    | term

def term[$: P]: P[Expr] =
  index("True").map((x, _) => LogiLit(x, true))
    | index("False").map((x, _) => LogiLit(x, false))
    | index(number).map(IntLit.apply)
    | string.map(StringLit.apply)
    | "(" ~ expr ~ ")"
    | call
    | loc.map(x => Loc(x.ind, x))

def string[$: P] = 
  import NoWhitespace.*
  index("\"" ~ CharPred(_ != '"').rep.! ~ "\"")

def call[$: P]: P[Expr] =
  (id ~ "(" ~ expr.rep(sep = ",") ~ ")").map((a, b) => Call(a.ind, a, b))

def typepar[$: P]: P[PType] = P("void" | "integer" | "logical").!.map:
  case "void"    => PType.Void
  case "integer" => PType.Integer
  case "logical" => PType.Logical

enum Temp:
  case Tmp(ind: Int, name: String) extends Temp
  case TmpTuple(ind: Int, lhs: Tmp, rhs: Temp) extends Temp

private def tloc[$: P]: P[Temp] =
  import Temp.*
  (index(ident) ~ ":" ~ tloc).map((i, s, l) => TmpTuple(i, Tmp(i, s), l))
    | index(ident).map(Tmp.apply)

private def toLocation(t: Temp): Location =
  import Temp.*, Location.*
  def go(t: Temp): Location = t match
    case TmpTuple(i, lhs, rhs) => go2(rhs, Var(i, lhs.name))
    case Tmp(ind, name)        => Var(ind, name)
  def go2(t: Temp, acc: Location): Location = t match
    case TmpTuple(i, lhs, rhs) => go2(rhs, Tuple(i, acc, Var(i, lhs.name)))
    case Tmp(ind, name)        => Tuple(ind, acc, Var(ind, name))
  go(t)

// turn from right recursive to left recursive
def loc[$: P]: P[Location] = tloc.map(toLocation)

// lexical token parsers
inline def token[$: P](inline s: String): P[String] = P(StringIn(s).!)

def number[$: P] =
  import NoWhitespace.*
  P(CharIn("0-9").rep(1).!.map(_.toInt))

def ident[$: P] =
  import NoWhitespace.*
  P(CharIn("a-zA-Z_") ~ CharIn("a-zA-Z0-9_").rep).!

// helper functions
extension [$: P, T](p: P[T]) def >>[V](v: => V): P[V] = p.map(_ => v)

inline def binop[$: P](
    inline subexp: P[Expr],
    inline ops: P[String],
    inline alt: P[Expr]
): P[Expr] = (alt ~ (ops ~/ subexp).rep).map((exp, rhss) =>
  rhss.foldLeft(exp):
    case (lhs, (op, rhs)) => BinOp(lhs.ind, toBinop(op), lhs, rhs)
)

inline def binopR[$: P](
    inline subexp: P[Expr],
    inline ops: P[String],
    inline alt: P[Expr]
): P[Expr] = ((subexp ~ ops).rep ~ alt).map((lhss, exp) =>
  lhss.foldRight(exp):
    case ((lhs, op), rhs) => BinOp(lhs.ind, toBinop(op), lhs, rhs)
)

import BinaryOp.*

val toBinop = Map(
  "+" -> Add,
  "-" -> Sub,
  "*" -> Mul,
  "/" -> Div,
  "==" -> Eq,
  "~=" -> Neq,
  "<" -> Lt,
  ">" -> Gt,
  "<=" -> Le,
  ">=" -> Ge,
  "&" -> And,
  "|" -> Or
)

import UnaryOp.*
val toUnop = Map("-" -> Neg, "~" -> Not)