package baselang;

// AST types
type Pos = (Int, Int)
type Body = (Seq[VarDecl], Seq[Stmt])
object Body:
  def empty: Body = (Nil, Nil)

enum VarDecl:
  case VDecl(ttype: PType, name: Location.Var) extends VarDecl
  case TupVDecl(ttype: Location.Var, name: Location.Var) extends VarDecl

enum TopLevel:
  case VDecl(val vdecl: VarDecl) extends TopLevel
  case FunDecl(
      retType: PType,
      name: Location.Var,
      params: Seq[VarDecl.VDecl],
      body: Body
  ) extends TopLevel
  case TupDecl(name: Location.Var, fields: Seq[VarDecl]) extends TopLevel

enum Stmt:
  case If(cond: Expr, thenStmt: Body, elseStmt: Option[Body]) extends Stmt
  case While(cond: Expr, body: Body) extends Stmt
  case Return(value: Option[Expr]) extends Stmt
  case ExprStmt(expr: Expr) extends Stmt
  case Write(expr: Expr) extends Stmt
  case Read(loc: Location) extends Stmt
  case Incr(loc: Location) extends Stmt
  case Decr(loc: Location) extends Stmt

enum Expr(var ind: Int, var myType: VType = null):
  case IntLit(i: Int, value: Int) extends Expr(i)
  case LogiLit(i: Int, value: Boolean) extends Expr(i)
  case StringLit(i: Int, value: String) extends Expr(i)
  case Loc(i: Int, loc: Location) extends Expr(i)
  case Call(i: Int, name: Location.Var, args: Seq[Expr]) extends Expr(i)
  case UnOp(i: Int, op: UnaryOp, rhs: Expr) extends Expr(i)
  case BinOp(i: Int, op: BinaryOp, lhs: Expr, rhs: Expr) extends Expr(i)
  case Assign(i: Int, lhs: Location, rhs: Expr) extends Expr(i)

enum Location(var ind: Int, var sym: Sym = null, var isGlobal: Boolean = false):
  case Var(i: Int, name: String) extends Location(i)
  case Tuple(i: Int, lhs: Location, rhs: Var) extends Location(i)

enum UnaryOp:
  case Neg, Not

enum BinaryOp:
  case Add, Sub, Mul, Div, Eq, Neq, Lt, Gt, Le, Ge, And, Or

enum PType:
  case Void, Integer, Logical

def mkString(program: Seq[TopLevel]): String =
  program.map(mkString).mkString("")

def mkString(tdecl: TopLevel): String = tdecl match
  case TopLevel.VDecl(vdecl) => mkString(vdecl, 0)
  case TopLevel.FunDecl(retType, name, params, body) =>
    s"$retType ${mkString(name)}(${params.map(mkString).mkString(", ")}) [\n${mkString(body, 1)}]\n"
  case TopLevel.TupDecl(name, fields) =>
    s"tuple $name {\n"
      + fields.map(mkString(_, 1)).mkString("")
      + "}.\n"

// unparse ast
def mkString(param: VarDecl.VDecl): String =
  val VarDecl.VDecl(ttype, name) = param
  s"$ttype ${mkString(name)}."

def mkString(vdecl: VarDecl, ilvl: Int): String = vdecl match
  case VarDecl.VDecl(ttype, name) => s"$ttype ${mkString(name)}.".indent(ilvl)
  case VarDecl.TupVDecl(ttype, name) =>
    s"tuple ${mkString(ttype)} ${mkString(name)}.".indent(ilvl)

def mkString(b: Body, ilvl: Int): String =
  val (vdecls, stmts) = b
  s"${vdecls.map(mkString(_, ilvl + 1)).mkString("")}" +
    s"${stmts.map(mkString(_, ilvl + 1)).mkString("")}"

def mkString(stmt: Stmt, ilvl: Int): String =
  import Stmt.*
  stmt match
    case If(cond, thenStmt, elseStmt) => s"if ${mkString(cond)} [".indent(ilvl)
        + mkString(thenStmt, ilvl + 1)
        + "]".indent(ilvl) + elseStmt.fold("") { elseStmt =>
          s"else [".indent(ilvl)
            + mkString(elseStmt, ilvl + 1)
            + "]".indent(ilvl)
        }
    case While(cond, body) => s"while [${mkString(cond)}".indent(ilvl)
        + mkString(body, ilvl + 1)
        + "]".indent(ilvl)
    case Return(value)  => s"return ${value.fold("")(mkString)}.".indent(ilvl)
    case ExprStmt(expr) => s"${mkString(expr)}.".indent(ilvl)
    case Write(expr)    => s"write << ${mkString(expr)}.".indent(ilvl)
    case Read(loc)      => s"read >> ${mkString(loc)}.".indent(ilvl)
    case Incr(loc)      => s"${mkString(loc)}++.".indent(ilvl)
    case Decr(loc)      => s"${mkString(loc)}--.".indent(ilvl)

def mkString(expr: Expr): String =
  import Expr.*
  expr match
    case IntLit(_, value)    => s"$value"
    case LogiLit(_, value)   => s"$value"
    case StringLit(_, value) => s"\"$value\""
    case Loc(_, loc)         => mkString(loc)
    case Call(_, name, args) =>
      s"${mkString(name)}(${args.map(mkString).mkString(", ")})"
    case UnOp(_, op, rhs) => s"${mkString(op)}${mkString(rhs)}"
    case BinOp(_, op, lhs, rhs) =>
      s"(${mkString(lhs)} ${mkString(op)} ${mkString(rhs)})"
    case Assign(_, lhs, rhs) => s"(${mkString(lhs)} = ${mkString(rhs)})"

def mkString(loc: Location): String = loc match
  case Location.Var(_, name) => name ++ dispType(loc.sym)
  case Location.Tuple(_, lhs, rhs) =>
    s"${mkString(lhs)}:${mkString(rhs)}" ++ dispType(loc.sym)

def dispType(s: Sym): String = s match
  case VarSym(ttype, g, o) => s"<$ttype>($g, $o)"
  case TupVarSym(ttype)    => s"<$ttype>"
  case FunSym(retType, params) =>
    s"<${params.map(_.ttype.toString).mkString("(", ", ", ")")} -> $retType>"
  case TupSym(fields) => "<internal error>"
  case null           => ""

def mkString(op: UnaryOp | BinaryOp): String = op match
  case UnaryOp.Neg  => "-"
  case UnaryOp.Not  => "~"
  case BinaryOp.Add => "+"
  case BinaryOp.Sub => "-"
  case BinaryOp.Mul => "*"
  case BinaryOp.Div => "/"
  case BinaryOp.Eq  => "=="
  case BinaryOp.Neq => "~="
  case BinaryOp.Lt  => "<"
  case BinaryOp.Gt  => ">"
  case BinaryOp.Le  => "<="
  case BinaryOp.Ge  => ">="
  case BinaryOp.And => "&"
  case BinaryOp.Or  => "|"
