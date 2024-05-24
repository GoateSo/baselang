package baselang

import scala.collection.mutable.ListBuffer

class Typecheck(s: State):
  /** verifies that a program is well-typed given the symbol table from name analysis
    *
    * @param prog
    *   the program to typecheck
    * @return
    *   the list of errors found during typechecking
    */
  def apply(prog: Seq[TopLevel]): ErrList =
    prog.foldLeft(Nil)(_ ++ typecheck(_))

  // for top level declarations (i.e. only function declarations)
  def typecheck(tdecl: TopLevel): ErrList = tdecl match
    case TopLevel.FunDecl(retType, name, params, body) =>
      val (errs, ret) = checkBlock(body, name.sym.asInstanceOf[FunSym])
      if retType != PType.Void && !ret then
        errs :+ s"Function ${name.name} does not return" -> name.ind
      else errs
    case _ => Nil

  // typecheck a block and return errors and whether it returns
  def checkBlock(body: Body, fun: FunSym): (ErrList, Boolean) =
    body._2.foldLeft(Nil, false):
      case ((errs, ret), stmt) =>
        val (e, r) = typecheck(stmt, fun)
        (errs ++ e, ret || r)

  // for statements, pass the embedded fuction name (for return statement)
  def typecheck(stmt: Stmt, fun: FunSym): (ErrList, Boolean) =
    import Stmt.*, VType.*
    def checkCond(cond: Expr): ErrList =
      val (t, cerrs) = typecheck(cond)
      val terr =
        if t != Logical
        then Seq("Non-logical condition" -> cond.ind)
        else Nil
      cerrs ++ terr
    stmt match
      case If(cond, thenStmt, elseStmt) =>
        val (t, cerrs)       = typecheck(cond)
        val (thenErrs, ret1) = checkBlock(thenStmt, fun)
        val (elseErrs, ret2) = elseStmt.fold(Nil, false)(checkBlock(_, fun))
        (cerrs ++ checkCond(cond) ++ thenErrs ++ elseErrs, ret1 || ret2)
      case While(cond, body) =>
        val (t, cerrs)  = typecheck(cond)
        val (errs, ret) = checkBlock(body, fun)
        (cerrs ++ checkCond(cond) ++ errs, ret)
      case Return(value) =>
        val (vType, errs) = value.fold(Void, Nil)(typecheck)
        if vType == fun.retType.toVType
        then (errs, true)
        else (errs :+ s"Return type mismatch" -> value.fold(0)(_.ind), true)
      case ExprStmt(expr) => (typecheck(expr)._2, false)
      case Write(expr) =>
        val (t, errs) = typecheck(expr)
        val nerrs = t match
          case Void      => errs :+ "can't write void" -> expr.ind
          case Fun(_, _) => errs :+ "can't write function" -> expr.ind
          case Tup(_)    => errs :+ "can't write tuple" -> expr.ind
          case _         => errs
        (nerrs, false)
      case Read(loc) =>
        val (t, errs) = typecheck(loc)
        val nerrs = t match
          case Void      => errs :+ "can't read to void" -> loc.ind
          case Fun(_, _) => errs :+ "can't read to function" -> loc.ind
          case Tup(_)    => errs :+ "can't read to tuple" -> loc.ind
          case _         => errs
        (nerrs, false)
      case Incr(loc) =>
        val (t, errs) = typecheck(loc)
        if t != Integer then (errs :+ "Incrementing non-integer location" -> loc.ind, false)
        else (errs, false)
      case Decr(loc) =>
        val (t, errs) = typecheck(loc)
        if t != Integer then (errs :+ "Decrementing non-integer location" -> loc.ind, false)
        else (errs, false)

  // expects that two types match or one is an error already
  def badType(exp: VType, act: VType): Boolean =
    exp != act && act != VType.Error

  def checkCall(fn: Location, args: Seq[Expr]): ExpRes =
    import VType.*, Expr.*
    val (funtype, ferr)   = typecheck(fn)
    val (argTypes, perrs) = args.map(typecheck).unzip
    val nerrs             = ferr ++ perrs.flatten
    if funtype == Error then (Error, ferr)
    else
      funtype match
        case Fun(retType, params) =>
          if params.size != argTypes.size then
            (Error, nerrs :+ "Argument count mismatch" -> fn.ind)
          else if params == argTypes then (retType, nerrs)
          else
            val mismatches = params
              .zip(argTypes)
              .zipWithIndex
              .collect:
                case ((p, a), i) if p != a =>
                  (s"Argument type mismatch at $i", args(i).ind)
            (Error, nerrs ++ mismatches)
        case _ =>
          (Error, nerrs :+ s"Calling non-function ${Unparse(fn)}" -> fn.ind)

  // typecheck expression
  def typecheck(expr: Expr): ExpRes =
    import Expr.*, VType.*
    val (a, b) = expr match
      case IntLit(_, value)    => (Integer, Nil)
      case LogiLit(_, value)   => (Logical, Nil)
      case StringLit(_, value) => (Str, Nil)
      case Loc(loc)            => typecheck(loc)
      case Call(name, args)    => checkCall(name, args)
      case UnOp(op, rhs) =>
        val (t, errs) = typecheck(rhs)
        if t == Error then (Error, errs)
        else
          import UnaryOp.*
          op match
            case Neg =>
              if t == Integer then (Integer, errs)
              else (Error, errs :+ "Negating non-integer" -> rhs.ind)
            case Not =>
              if t == Logical then (Logical, errs)
              else (Error, errs :+ "logical not on non-logical" -> rhs.ind)
      case BinOp(op, lhs, rhs) =>
        val (lt, le) = typecheck(lhs)
        val (rt, re) = typecheck(rhs)
        if lt == Error && rt == Error then (Error, le ++ re)
        else
          import BinaryOp.*
          val errs = ListBuffer[(String, Int)]()
          // check that left and right match expected type, and give corresp. errors if mismatch
          inline def checkOp(
              inline exp: VType,
              inline res: VType,
              inline err: String
          ): ExpRes =
            if lt == exp && rt == exp then (res, le ++ re)
            else
              if badType(lt, exp) then errs += err -> lhs.ind
              if badType(rt, exp) then errs += err -> rhs.ind
              (Error, le ++ re ++ errs)
          op match
            case Add | Sub | Mul | Div =>
              checkOp(Integer, Integer, "arithmetic op on non-integer operand")
            case Lt | Gt | Le | Ge =>
              checkOp(Integer, Logical, "comparison on non-integer operand")
            case And | Or =>
              checkOp(Logical, Logical, "logical op on non-logical operand")
            case Eq | Neq =>
              if lt == rt then
                if lt == Integer || lt == Logical then (Logical, le ++ re)
                else (Error, le ++ re :+ "comp of non-primitive types" -> lhs.ind)
              else (Error, le ++ re :+ "Comparison of different types" -> lhs.ind)
      case Assign(lhs, rhs) =>
        // TODO: add checks for function and tuple
        val (lt, le) = typecheck(lhs)
        val (rt, re) = typecheck(rhs)
        val combined = le ++ re
        if lt == Error || rt == Error then (Error, combined)
        else if lt == rt then
          if lt == Integer || lt == Logical then (Logical, combined)
          else (Error, combined :+ s"assign to invalid type ($lt)" -> lhs.ind)
        else (Error, combined :+ "assign of different types" -> lhs.ind)
    expr.myType = a
    (a, b)
  // typecheck location
  def typecheck(loc: Location): ExpRes = (loc.sym.toVType, Nil)
end Typecheck
