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
  case Loc(i: Int, loc: Location) extends Expr(i)
  case Call(i: Int, name: Var, args: Seq[Expr]) extends Expr(i)
  case UnOp(i: Int, op: UnaryOp, rhs: Expr) extends Expr(i)
  case BinOp(i: Int, op: BinaryOp, lhs: Expr, rhs: Expr) extends Expr(i)
  case Assign(i: Int, lhs: Location, rhs: Expr) extends Expr(i)

enum Stmt:
  case If(cond: Expr, thenStmt: Body, elseStmt: Option[Body]) extends Stmt
  case While(cond: Expr, body: Body) extends Stmt
  case Return(value: Option[Expr]) extends Stmt
  case ExprStmt(expr: Expr) extends Stmt
  case Write(expr: Expr) extends Stmt
  case Read(loc: Location) extends Stmt
  case Incr(loc: Location) extends Stmt
  case Decr(loc: Location) extends Stmt

enum VarDecl(var size: Int = 0, val name: Var):
  case PrimDecl(ttype: PType, vname: Var) extends VarDecl(name = vname)
  case TupDecl(ttype: Var, vname: Var) extends VarDecl(name = vname)

enum TopLevel:
  case VarDec(val vdecl: VarDecl) extends TopLevel
  case FunDecl(
      retType: PType,
      name: Var,
      params: Seq[VarDecl.PrimDecl],
      body: Body
  ) extends TopLevel
  case TupDecl(name: Var, fields: Seq[VarDecl]) extends TopLevel

type Pos  = (Int, Int)
type Body = (Seq[VarDecl], Seq[Stmt])
object Body:
  def empty: Body = (Nil, Nil)
