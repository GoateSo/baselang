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

enum Location(var ind: Int, var sym: Sym = null):
  case Var(i: Int, name: String) extends Location(i)
  case Tuple(i: Int, lhs: Location, rhs: Var) extends Location(i)

enum UnaryOp:
  case Neg, Not

enum BinaryOp:
  case Add, Sub, Mul, Div, Eq, Neq, Lt, Gt, Le, Ge, And, Or

enum PType:
  case Void, Integer, Logical
