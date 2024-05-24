package baselang;

// AST types
enum UnaryOp:
  case Neg, Not

import UnaryOp.*
val toUnop = Map("-" -> Neg, "~" -> Not)

enum BinaryOp:
  case Add, Sub, Mul, Div, Eq, Neq, Lt, Gt, Le, Ge, And, Or
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
// converting back to strings
val fromBinop = toBinop.map(_.swap)

enum PType:
  case Void, Integer, Logical

enum Location(var ind: Int, var sym: Sym = null):
  case Var(i: Int, name: String) extends Location(i)
  case Tuple(i: Int, lhs: Location, rhs: Var) extends Location(i)
import Location.Var

enum Expr(var ind: Int, var myType: VType = null):
  case IntLit(i: Int, value: Int) extends Expr(i)
  case LogiLit(i: Int, value: Boolean) extends Expr(i)
  case StringLit(i: Int, value: String) extends Expr(i)
  case Loc(loc: Location) extends Expr(loc.ind)
  case Call(name: Var, args: Seq[Expr]) extends Expr(name.ind)
  case UnOp(op: UnaryOp, rhs: Expr) extends Expr(rhs.ind)
  case BinOp(op: BinaryOp, lhs: Expr, rhs: Expr) extends Expr(lhs.ind)
  case Assign(lhs: Location, rhs: Expr) extends Expr(lhs.ind)

enum Stmt:
  case If(cond: Expr, thenStmt: Body, elseStmt: Option[Body]) extends Stmt
  case While(cond: Expr, body: Body) extends Stmt
  case Return(value: Option[Expr]) extends Stmt
  case ExprStmt(expr: Expr) extends Stmt
  case Write(expr: Expr) extends Stmt
  case Read(loc: Location) extends Stmt
  case Incr(loc: Location) extends Stmt
  case Decr(loc: Location) extends Stmt

// TODO: move tuple creation to parsing, and add size field to Prim/TupDecl instead of doing so during name analysis
enum VarDecl(var size: Int = 0, val name: Var):
  case PrimDecl(ttype: PType, vname: Var) extends VarDecl(name = vname)
  case TVarDecl(ttype: Var, vname: Var) extends VarDecl(name = vname)
import VarDecl.*

enum TopLevel:
  case VarDec(val vdecl: VarDecl) extends TopLevel
  case FunDecl(ret: PType, name: Var, params: Seq[PrimDecl], body: Body) extends TopLevel
  case TupDecl(name: Var, fields: Seq[VarDecl]) extends TopLevel

type Pos  = (Int, Int)
type Body = (Seq[VarDecl], Seq[Stmt])
object Body:
  def empty: Body = (Nil, Nil)
