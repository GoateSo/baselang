package baselang;

object Unparse:

  def apply(program: Seq[TopLevel]): String =
    program.map(apply).mkString("")

  def apply(tdecl: TopLevel): String =
    import TopLevel.*
    tdecl match
      case VDecl(vdecl) => apply(vdecl, 0)
      case FunDecl(retType, name, params, body) =>
        s"$retType ${apply(name)}(${params.map(apply).mkString(", ")}) [\n${apply(body, 1)}]\n"
      case TupDecl(name, fields) =>
        s"tuple $name {\n"
          + fields.map(apply(_, 1)).mkString("")
          + "}.\n"

  // unparse ast
  def apply(param: VarDecl.VDecl): String =
    val VarDecl.VDecl(ttype, name) = param
    s"$ttype ${apply(name)}."

  def apply(vdecl: VarDecl, ilvl: Int): String = vdecl match
    case VarDecl.VDecl(ttype, name) => s"$ttype ${apply(name)}.".indent(ilvl)
    case VarDecl.TupVDecl(ttype, name) =>
      s"tuple ${apply(ttype)} ${apply(name)}.".indent(ilvl)

  def apply(b: Body, ilvl: Int): String =
    val (vdecls, stmts) = b
    s"${vdecls.map(apply(_, ilvl + 1)).mkString("")}" +
      s"${stmts.map(apply(_, ilvl + 1)).mkString("")}"

  def apply(stmt: Stmt, ilvl: Int): String =
    import Stmt.*
    stmt match
      case If(cond, thenStmt, elseStmt) => s"if ${apply(cond)} [".indent(ilvl)
          + apply(thenStmt, ilvl + 1)
          + "]".indent(ilvl) + elseStmt.fold("") { elseStmt =>
            s"else [".indent(ilvl)
              + apply(elseStmt, ilvl + 1)
              + "]".indent(ilvl)
          }
      case While(cond, body) => s"while [${apply(cond)}".indent(ilvl)
          + apply(body, ilvl + 1)
          + "]".indent(ilvl)
      case Return(value)  => s"return ${value.fold("")(apply)}.".indent(ilvl)
      case ExprStmt(expr) => s"${apply(expr)}.".indent(ilvl)
      case Write(expr)    => s"write << ${apply(expr)}.".indent(ilvl)
      case Read(loc)      => s"read >> ${apply(loc)}.".indent(ilvl)
      case Incr(loc)      => s"${apply(loc)}++.".indent(ilvl)
      case Decr(loc)      => s"${apply(loc)}--.".indent(ilvl)

  def apply(expr: Expr): String =
    import Expr.*
    expr match
      case IntLit(_, value)    => s"$value"
      case LogiLit(_, value)   => s"$value"
      case StringLit(_, value) => s"\"$value\""
      case Loc(_, loc)         => apply(loc)
      case Call(_, name, args) =>
        s"${apply(name)}(${args.map(apply).mkString(", ")})"
      case UnOp(_, op, rhs) => s"${apply(op)}${apply(rhs)}"
      case BinOp(_, op, lhs, rhs) =>
        s"(${apply(lhs)} ${apply(op)} ${apply(rhs)})"
      case Assign(_, lhs, rhs) => s"(${apply(lhs)} = ${apply(rhs)})"

  def apply(loc: Location): String = loc match
    case Location.Var(_, name) => name ++ dispType(loc.sym)
    case Location.Tuple(_, lhs, rhs) =>
      s"${apply(lhs)}:${apply(rhs)}" ++ dispType(loc.sym)

  def dispType(s: Sym): String = s match
    case VarSym(ttype, g, o) => s"<$ttype>($g, $o)"
    case TupVarSym(ttype)    => s"<$ttype>"
    case FunSym(retType, params) =>
      s"<${params.map(_.ttype.toString).mkString("(", ", ", ")")} -> $retType>"
    case TupSym(fields) => "<internal error>"
    case null           => ""

  import UnaryOp.*, BinaryOp.*
  def apply(op: UnaryOp | BinaryOp): String = op match
    case Neg => "-"
    case Not => "~"
    case Add => "+"
    case Sub => "-"
    case Mul => "*"
    case Div => "/"
    case Eq  => "=="
    case Neq => "~="
    case Lt  => "<"
    case Gt  => ">"
    case Le  => "<="
    case Ge  => ">="
    case And => "&"
    case Or  => "|"
end Unparse
