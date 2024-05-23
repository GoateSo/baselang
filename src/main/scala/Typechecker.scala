package baselang

import scala.collection.mutable.ListBuffer

enum VType:
  case Error, Void, Integer, Logical, Str
  case Fun(retType: VType, params: Seq[VType])
  case Tup(name: String)

type ExpRes = (VType, Seq[(String, Int)])

extension (tt: PType)
  def toVType: VType =
    import PType.*
    tt match
      case Void    => VType.Void
      case Integer => VType.Integer
      case Logical => VType.Logical

extension (sym: Sym)
  def toVType: VType =
    import VType.*
    sym match
      case VarSym(ttype, _, _) => ttype.toVType
      case TupVarSym(ttype)    => Tup(ttype)
      case FunSym(ret, params) => Fun(ret.toVType, params.map(_.toVType))
      case TupSym(fields)      => Error
      case null                => Error

def typecheck(prog: Seq[TopLevel], s: State): Seq[(String, Int)] =
  prog.foldLeft(Nil)(_ ++ typecheck(_, s))

// for top level declarations (i.e. only function declarations)
def typecheck(tdecl: TopLevel, s: State): Seq[(String, Int)] = tdecl match
  case TopLevel.FunDecl(retType, name, params, body) =>
    val (errs, ret) = checkBlock(body, name.sym.asInstanceOf[FunSym], s)
    if retType != PType.Void && !ret then
      errs :+ s"Function ${name.name} does not return" -> name.ind
    else errs
  case _ => Nil

// typecheck a block and return errors and whether it returns
def checkBlock(
    body: Body,
    fun: FunSym,
    s: State
): (Seq[(String, Int)], Boolean) = body._2.foldLeft((Nil, false)):
  case ((errs, ret), stmt) =>
    val (e, r) = typecheck(stmt, fun, s)
    (errs ++ e, ret || r)

// for statements, pass the embedded fuction name (for return statement)
def typecheck(
    stmt: Stmt,
    fun: FunSym,
    s: State
): (Seq[(String, Int)], Boolean) =
  import Stmt.*, VType.*
  def checkCond(cond: Expr): Seq[(String, Int)] =
    val (t, cerrs) = typecheck(cond, s)
    val terr =
      if t != Logical then Seq("Non-logical condition(1)" -> cond.ind) else Nil
    cerrs ++ terr
  stmt match
    case If(cond, thenStmt, elseStmt) =>
      val (t, cerrs) = typecheck(cond, s)
      val (thenErrs, ret1) = checkBlock(thenStmt, fun, s)
      val (elseErrs, ret2) = elseStmt.fold((Nil, false))(checkBlock(_, fun, s))
      (cerrs ++ checkCond(cond) ++ thenErrs ++ elseErrs, ret1 || ret2)
    case While(cond, body) =>
      val (t, cerrs) = typecheck(cond, s)
      val (errs, ret) = checkBlock(body, fun, s)
      (cerrs ++ checkCond(cond) ++ errs, ret)
    case Return(value) =>
      val (vType, errs) = value.fold(Void, Nil)(typecheck(_, s))
      if vType == fun.retType.toVType
      then (errs, true)
      else (errs :+ s"Return type mismatch" -> value.fold(0)(_.ind), true)
    case ExprStmt(expr) => (typecheck(expr, s)._2, false)
    case Write(expr) =>
      val (t, errs) = typecheck(expr, s)
      val nerrs = t match
        case Void      => errs :+ "can't write void" -> expr.ind
        case Fun(_, _) => errs :+ "can't write function" -> expr.ind
        case Tup(_)    => errs :+ "can't write tuple" -> expr.ind
        case _         => errs
      (nerrs, false)
    case Read(loc) =>
      val (t, errs) = typecheck(loc, s)
      val nerrs = t match
        case Void      => errs :+ "can't read to void" -> loc.ind
        case Fun(_, _) => errs :+ "can't read to function" -> loc.ind
        case Tup(_)    => errs :+ "can't read to tuple" -> loc.ind
        case _         => errs
      (nerrs, false)
    case Incr(loc) =>
      val (t, errs) = typecheck(loc, s)
      if t != Integer then
        (errs :+ "Incrementing non-integer location" -> loc.ind, false)
      else (errs, false)
    case Decr(loc) =>
      val (t, errs) = typecheck(loc, s)
      if t != Integer then
        (errs :+ "Decrementing non-integer location" -> loc.ind, false)
      else (errs, false)

// expects that two types match or one is an error already
def badType(exp: VType, act: VType): Boolean =
  if exp == act || act == VType.Error then false
  else true

def checkCall(fn: Location, args: Seq[Expr], s: State): ExpRes =
  import VType.*, Expr.*
  val (funtype, ferr) = typecheck(fn, s)
  val (argTypes, perrs) = args.map(typecheck(_, s)).unzip
  val nerrs = ferr ++ perrs.flatten
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
def typecheck(expr: Expr, s: State): ExpRes =
  import Expr.*, VType.*
  val (a, b) = expr match
    case IntLit(_, value)    => (Integer, Nil)
    case LogiLit(_, value)   => (Logical, Nil)
    case StringLit(_, value) => (Str, Nil)
    case Loc(_, loc)         => typecheck(loc, s)
    case Call(_, name, args) => checkCall(name, args, s)
    case UnOp(_, op, rhs) =>
      val (t, errs) = typecheck(rhs, s)
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
    case BinOp(_, op, lhs, rhs) =>
      val (lt, le) = typecheck(lhs, s)
      val (rt, re) = typecheck(rhs, s)
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
    case Assign(_, lhs, rhs) =>
      // TODO: add checks for function and tuple
      val (lt, le) = typecheck(lhs, s)
      val (rt, re) = typecheck(rhs, s)
      val combined = le ++ re
      if lt == Error || rt == Error then (Error, combined)
      else if lt == rt then
        if lt == Integer || lt == Logical then (Logical, combined)
        else (Error, combined :+ s"assign to invalid type ($lt)" -> lhs.ind)
      else (Error, combined :+ "assign of different types" -> lhs.ind)
  expr.myType = a
  (a, b)
// typecheck location
def typecheck(loc: Location, s: State): ExpRes = (loc.sym.toVType, Nil)
