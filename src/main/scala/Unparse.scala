package baselang;

object Unparse:
  /** Converts a program in AST form back into text, including any type information added during
    * analysis
    *
    * @param program
    *   parsed AST for program
    * @return
    *   string representation of the program
    */
  def apply(program: Seq[TopLevel]): String =
    program.map(apply).mkString

  import TopLevel.*
  def apply(tdecl: TopLevel): String = tdecl match
    case VarDec(vdecl) => apply(vdecl, 0)
    case FunDecl(retType, name, params, body) =>
      val ps = params.map { case VarDecl.PrimDecl(a, b) => s"$a ${apply(b)}" }.mkString(", ")
      s"$retType ${apply(name)}($ps}) [\n${apply(body, 1)}]\n"
    case TupDecl(name, fields) =>
      s"tuple ${apply(name)} {\n"
        + fields.map(apply(_, 1)).mkString
        + "}.\n"

  def apply(vdecl: VarDecl, ilvl: Int): String = vdecl match
    case VarDecl.PrimDecl(ttype, name) => s"$ttype ${apply(name)}.".indent(ilvl)
    case VarDecl.TVarDecl(ttype, name) =>
      s"tuple ${apply(ttype)} ${apply(name)}.".indent(ilvl)

  def apply(b: Body, ilvl: Int): String =
    val (vdecls, stmts) = b
    s"${vdecls.map(apply(_, ilvl + 1)).mkString}" +
      s"${stmts.map(apply(_, ilvl + 1)).mkString}"

  import Stmt.*
  def apply(stmt: Stmt, ilvl: Int): String = stmt match
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

  import Expr.*
  def apply(expr: Expr): String = expr match
    case IntLit(_, value)    => s"$value"
    case LogiLit(_, value)   => s"$value"
    case StringLit(_, value) => s"\"$value\""
    case Loc(loc)            => apply(loc)
    case Call(name, args) =>
      s"${apply(name)}(${args.map(apply).mkString(", ")})"
    case UnOp(op, rhs) => s"${apply(op)}${apply(rhs)}"
    case BinOp(op, lhs, rhs) =>
      s"(${apply(lhs)} ${apply(op)} ${apply(rhs)})"
    case Assign(lhs, rhs) => s"(${apply(lhs)} = ${apply(rhs)})"

  def apply(loc: Location): String = loc match
    case Location.Var(_, name) => name ++ dispType(loc.sym)
    case Location.Tuple(_, lhs, rhs) =>
      s"${apply(lhs)}:${apply(rhs)}" ++ dispType(loc.sym)

  def dispType(s: Sym): String = s match
    case VarSym(ttype, _)       => s"<$ttype>"
    case TupVarSym(ttype, t, _) => s"<$ttype>"
    case FunSym(ret, ps)        => s"<${ps.map(_.ttype).mkString("(", ", ", ")")} -> $ret>"
    case TupSym(fields, _)      => "<internal error>"
    case null                   => ""

  import UnaryOp.*, BinaryOp.*
  def apply(op: BinaryOp): String = fromBinop(op)
  def apply(op: UnaryOp): String = op match
    case Neg => "-"
    case Not => "~"
end Unparse
