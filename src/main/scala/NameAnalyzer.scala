package baselang;
import scala.collection.mutable.{ListBuffer, Map}

class NameAnalyzer(state: State):
  import Location.Var, TopLevel.*
  def analyze(prog: Seq[TopLevel]): (ErrList, State) =
    state.clear()
    val res = prog.foldLeft(Nil)(_ ++ analyze(_))
    if state.lookupGlobal("main").isEmpty then (res :+ "No main function declared" -> 0, state)
    else (res, state)

  def analyze(decl: TopLevel): ErrList =
    val buffer = ListBuffer[Err]()
    val sym = decl match
      // ====================== variable declaration ======================
      case VarDec(vdec) => buffer ++= processVar(vdec, true)
      // ====================== function declaration ========================
      case FunDecl(retType, name, params, body) =>
        // check for unique name
        val fname = name.name
        if state.hasConflict(fname) then buffer += s"name '$name' already in use" -> name.ind
        // get parameter types/offsets and push function signature (if no name conflict)
        val paramtypes = params.zipWithIndex.map((x, i) => VarSym(x.ttype, (false, 4 + 4 * i)))
        val nsym       = FunSym(retType, paramtypes)
        name.sym = nsym
        // push function signature into enclosing scope
        if buffer.isEmpty then state += (fname, nsym)
        val bodyErrors = state.enclose:
          // check that parameters are unique and push into enclosed scope
          for case (
              VarDecl.PrimDecl(_, Location.Var(i, n)),
              psym
            ) <- params.zip(paramtypes)
          do
            if state.hasConflict(n)
            then buffer += s"Variable (parameter) $n already declared" -> i
            else state += (n, psym)
          state.offset = -8 // set offset for the function body to use
          analyzeBlock(body) // adds another enclosing scope so shadowing parameters is allowed
        if bodyErrors.nonEmpty then buffer ++= bodyErrors
      // ========================= Tuple declaration =========================
      case TupDecl(Location.Var(i, name), fields) =>
        val local = Map[String, Sym]() // local field env
        val errs  = ListBuffer[Err]()
        // check for duplicate fields and type errors -- additionally keep track of offset + size information
        var offset = 0
        for field <- fields do
          val fieldErrs = ListBuffer[Err]()
          import VarDecl.*, PType.*
          val loc = field.name
          if local.contains(loc.name) then
            fieldErrs += s"field ${loc.name} already declared" -> loc.ind
          val (size, sym) = field match
            case PrimDecl(t, Var(i, n)) => // primitive field
              if t == Void then fieldErrs += s"field $n cant be of type void" -> i
              (4, VarSym(t, (false, offset)))
            case TVarDecl(Var(ti, tn), _) => // tuple type field
              if !state.hasTuple(tn) then fieldErrs += s"Tuple $tn not declared" -> ti
              val tup = state.getTuple(tn)
              (tup.size, TupVarSym(tn, tup, (false, offset)))
          // if no errors, update local field environment and offset
          if fieldErrs.isEmpty then
            loc.sym = sym
            local += loc.name -> sym
          offset += size
        val sym = TupSym(local, offset)
        if state.hasTuple(name)
        then buffer += s"Tuple $name already declared" -> i
        else state.addTuple(name -> sym)
    buffer.toList

  // called by toplevel declarations and local variable declarations
  def processVar(vdecl: VarDecl, isGlobal: Boolean): ErrList =
    val errs = ListBuffer[Err]()
    val loc  = vdecl.name // get associated variable from VarDecl
    if state.hasConflict(loc.name) then
      errs += s"Variable ${loc.name} already declared" -> loc.ind
    val sym = vdecl match // check for errors and create symbol
      case VarDecl.PrimDecl(ttype, l @ Location.Var(i, name)) =>
        vdecl.size = 4
        if ttype == PType.Void then errs += "Cannot declare variable of type void" -> i
        if state.hasConflict(name) then errs += s"Variable $name already declared" -> i
        val offset = if isGlobal then 1 else state.nextVarOffset(4)
        VarSym(ttype, (isGlobal, offset))
      case VarDecl.TVarDecl(Location.Var(ti, ttype), l @ Location.Var(i, name)) =>
        if !state.hasTuple(ttype) then errs += s"Tuple $ttype not declared" -> ti
        val tup   = state.getTuple(ttype)
        val tsize = tup.size
        vdecl.size = tsize
        val offset = if isGlobal then 1 else state.nextVarOffset(tsize)
        TupVarSym(ttype, tup, (isGlobal, offset))
    loc.sym = sym
    // associate symbol with name if no errors
    if errs.isEmpty then state += (loc.name, sym)
    errs.toList
  // TODO: refactor out the common code between process and TupDecl's fields into some checkLocals function

  // errors and linked symbol information information
  // TODO clean up this and its uses
  def analyzeLoc(lval: Location): (ErrList, Option[Sym]) = lval match
    case Location.Var(ind, name) => state.lookupGlobal(name) match
        case Some(sym) =>
          lval.sym = sym
          (Nil, Some(sym))
        case None => (Seq(s"Variable $name not declared" -> ind), None)
    case Location.Tuple(_, lhs, Location.Var(ind, name)) =>
      analyzeLoc(lhs) match
        case (Nil, Some(TupVarSym(tt, tup, _))) =>
          tup.fields.get(name) match
            case Some(sym) =>
              lval.sym = sym
              (Nil, Some(sym))
            case None => (Seq(s"Field $name not declared in tuple $tt" -> ind), None)
        case (Nil, Some(VarSym(t, _))) => (Seq(s"attempt to index $t" -> ind), None)
        case (err, _)                  => (err, None)

  import Stmt.*
  def analyze(stmt: Stmt): ErrList = stmt match
    case If(cond, thenStmt, elseStmt) => analyze(cond)
        ++ analyzeBlock(thenStmt)
        ++ elseStmt.fold(Nil)(analyzeBlock)
    case While(cond, body) => analyze(cond) ++ analyzeBlock(body)
    case Return(value)     => value.fold(Nil)(analyze)
    case ExprStmt(expr)    => analyze(expr)
    case Write(expr)       => analyze(expr)
    case Read(loc)         => analyzeLoc(loc)._1
    case Incr(loc)         => analyzeLoc(loc)._1
    case Decr(loc)         => analyzeLoc(loc)._1

  import Expr.*
  def analyze(expr: Expr): ErrList = expr match
    case Call(name, args)   => analyzeLoc(name)._1 ++ args.flatMap(analyze)
    case Loc(loc)           => analyzeLoc(loc)._1
    case UnOp(_, rhs)       => analyze(rhs)
    case BinOp(_, lhs, rhs) => analyze(lhs) ++ analyze(rhs)
    case Assign(lhs, rhs)   => analyzeLoc(lhs)._1 ++ analyze(rhs)
    case _                  => Nil

  def analyzeBlock(body: Body): ErrList =
    state.enclose:
      body._1.foldLeft(Nil)(_ ++ processVar(_, false))
        ++ body._2.foldLeft(Nil)(_ ++ analyze(_))
end NameAnalyzer
