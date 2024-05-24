package baselang;

import fastparse.*, SingleLineWhitespace.*

// get line and column number from index and a given program
def lineCol(index: Int, input: String): (Int, Int) =
  val relevant = input.take(index)
  val lines    = relevant.count(_ == '\n') + 1
  val lastNL   = relevant.lastIndexOf('\n')
  val col      = index - lastNL
  (lines, col)

inline def index[$: P, T](inline p: P[T]): P[(Int, T)] = P(Index ~ p)

def id[$: P]: P[Location.Var] = // identifier (decorated)
  import NoWhitespace.*
  index(ident).map(Location.Var.apply)

def program[$: P]: P[Seq[TopLevel]] = // program
  import MultiLineWhitespace.*
  Start ~ decl.rep ~ End

def decl[$: P]: P[TopLevel] = // top level declarations
  import TopLevel.*
  def param[$: P]: P[VarDecl.PrimDecl] =
    (ptype ~ id).map(VarDecl.PrimDecl.apply)
  def fundecl[$: P]: P[TopLevel] =
    import MultiLineWhitespace.*
    (ptype ~/ id ~/ "{" ~ param.rep(sep = ",") ~ "}" ~ block)
      .map(FunDecl.apply)
  def tupdecl[$: P]: P[TopLevel] =
    import MultiLineWhitespace.*
    ("tuple" ~ id ~ "{" ~ vdecl.rep ~ "}" ~ ".").map(TupDecl.apply)

  P((vdecl.map(VarDec.apply) | fundecl | tupdecl) ~ comment.?)

// statements and blocks
def comment[$: P]: P[Unit] = // comments
  import NoWhitespace.*
  P(("$" | "!!") ~ CharsWhile(_ != '\n').rep ~ "\n")

def ifstmt[$: P]: P[Stmt] = // if statement
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
  ) ~ comment.?

def vdecl[$: P]: P[VarDecl] = // variable declaration
  import MultiLineWhitespace.*, VarDecl.*
  def tuple    = ("tuple" ~ id ~ id ~ "." ~ comment.?) map (TVarDecl.apply)
  def variable = (ptype ~ id ~ "." ~ comment.?) map (PrimDecl.apply)
  P(tuple | variable)

private inline def list[$: P, T](inline p: P[T]): P[Seq[T]] =
  import MultiLineWhitespace.*
  (p | comment).rep.map:
    _.flatMap:
      case x: T => Seq(x)
      case _    => Nil

def block[$: P]: P[Body] = // general block parser
  import MultiLineWhitespace.*
  P("[" ~ list(vdecl) ~ list(stmt) ~ "]" ~ comment.?)

import Expr.*
// expressions - seperated for precedence
def expr[$: P]            = assign | or
def assign[$: P]: P[Expr] = (loc ~ "=" ~ expr) map (Assign.apply)
def or[$: P]              = binop(and, "|".!)
def and[$: P]             = binop(compare, "&".!)
def compare[$: P]         = binop(addsub, StringIn("==", "~=", "<", ">", "<=", ">=").!)
def addsub[$: P]: P[Expr] = binop(muldiv, CharIn("+\\-").!)
def muldiv[$: P]: P[Expr] = binop(unary, CharIn("*/").!)
def unary[$: P]: P[Expr] =
  "-" ~ unary.map(UnOp(UnaryOp.Neg, _))
    | "~" ~ unary.map(UnOp(UnaryOp.Not, _))
    | term
def term[$: P]: P[Expr] = // term parser
  index("True").map((x, _) => LogiLit(x, true))
    | index("False").map((x, _) => LogiLit(x, false))
    | index(number).map(IntLit.apply)
    | string.map(StringLit.apply)
    | "(" ~ expr ~ ")"
    | call
    | loc.map(Loc.apply)
def string[$: P] = // string literal parser
  import NoWhitespace.*
  index("\"" ~ CharPred(_ != '"').rep.! ~ "\"")

def call[$: P]: P[Expr] = // function call parser
  (id ~ "(" ~ expr.rep(sep = ",") ~ ")").map(Call.apply)

def ptype[$: P]: P[PType] = // type parser
  import PType.*
  P("void" >> Void | "integer" >> Integer | "logical" >> Logical)

enum Temp:
  case Tmp(ind: Int, name: String) extends Temp
  case TmpTuple(ind: Int, lhs: Tmp, rhs: Temp) extends Temp

private def tloc[$: P]: P[Temp] =
  import Temp.*
  (index(ident) ~ ":" ~ tloc).map((i, s, l) => TmpTuple(i, Tmp(i, s), l))
    | index(ident).map(Tmp.apply)

// convert between the right recursive Temp and left recursive Location
private def toLocation(t: Temp): Location =
  import Temp.*, Location.*
  def go(t: Temp, acc: Location): Location = t match
    case TmpTuple(i, lhs, rhs) => go(rhs, Tuple(i, acc, Var(i, lhs.name)))
    case Tmp(ind, name)        => Tuple(ind, acc, Var(ind, name))
  t match
    case TmpTuple(i, lhs, rhs) => go(rhs, Var(i, lhs.name))
    case Tmp(ind, name)        => Var(ind, name)
// turn from right recursive to left recursive
def loc[$: P]: P[Location] = tloc.map(toLocation)

// lexical token parsers
inline def token[$: P](inline s: String): P[String] = P(s.!)

def number[$: P] =
  import NoWhitespace.*
  P(CharIn("0-9").rep(1).!.map(_.toInt))

def ident[$: P] =
  import NoWhitespace.*
  P(CharIn("a-zA-Z_") ~ CharIn("a-zA-Z0-9_").rep).!

// helper functions
extension [$: P, T](p: P[T]) def >>[V](v: => V): P[V] = p.map(_ => v)

// parse left associative binary operators
inline def binop[$: P](
    inline subexp: P[Expr],
    inline ops: P[String]
): P[Expr] = (subexp ~ (ops ~/ subexp).rep).map: (exp, rhss) =>
  rhss.foldLeft(exp):
    case (lhs, (op, rhs)) => BinOp(toBinop(op), lhs, rhs)
