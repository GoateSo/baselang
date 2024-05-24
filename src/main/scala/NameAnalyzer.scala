package baselang;
import scala.collection.mutable.{ListBuffer, Map}

type ErrList = Seq[(String, Int)]

def error(msg: String) = throw new Exception(msg)

sealed trait Sym(val isGlobal: Boolean = false, var offset: Int = 0)
case class VarSym(ttype: PType, glob: Boolean, off: Int, size: Int = 4) extends Sym(glob, off)
case class TupVarSym(ttype: String, tup: TupSym, glob: Boolean, off: Int, size: Int)
    extends Sym(glob, off)
case class FunSym(retType: PType, params: Seq[VarSym]) extends Sym
case class TupSym(fields: Map[String, Sym], val size: Int) extends Sym

case class State(
    val vars: ListBuffer[Map[String, Sym]] = ListBuffer(Map()),
    val tuples: Map[String, TupSym] = Map(),
    var offset: Int = 0
):
  def pushScope(): Unit = vars += Map()
  def popScope(): Unit  = vars.dropRightInPlace(1)
  def enclose[T](body: => T): T =
    pushScope()
    val res = body
    popScope()
    res
  def +=(name: String, sym: Sym): Unit = vars.last += (name -> sym)
  def lookupGlobal(name: String): Option[Sym] =
    vars.findLast(_.contains(name)).flatMap(_.get(name))
  def lookupLocal(name: String): Option[Sym] = vars.last.get(name)
  def hasTuple(name: String): Boolean        = tuples.contains(name)
  def getTuple(name: String): TupSym         = tuples(name)
  def addTuple(tup: (String, TupSym)): Unit  = tuples += tup
  def nextVarOffset(incr: Int): Int =
    offset -= incr
    offset

def analyze(prog: Seq[TopLevel]): (ErrList, State) =
  val state = State()
  val res   = prog.foldLeft(Nil)(_ ++ analyze(_, state))
  if state.lookupGlobal("main").isEmpty then (res :+ "No main function declared" -> 0, state)
  else (res, state)

def analyze(decl: TopLevel, state: State): ErrList =
  val buffer = ListBuffer[(String, Int)]()
  val sym = decl match
    // ====================== variable declaration ======================
    case TopLevel.VarDec(vdec) =>
      val (s, errs) = processVar(vdec, state, true)
      buffer ++= errs
    // ====================== function declaration ========================
    case TopLevel.FunDecl(retType, name, params, body) =>
      // check for unique name
      var badDefn = false
      val str     = name.name
      if state.lookupGlobal(str).isDefined then
        buffer += s"name '$name' already in use" -> name.ind
        badDefn = true
      // get parameter types/offsets and push function signature (if no name conflict)
      val paramtypes =
        params.zipWithIndex.map((x, i) => VarSym(x.ttype, false, 4 + 4 * i))
      val nsym = FunSym(retType, paramtypes)
      name.sym = nsym
      // push function signature into enclosing scope
      if !badDefn then state += (str, nsym)
      val bodyErrors = state.enclose:
        // check that parameters are unique and push into enclosed scope
        for case (
            VarDecl.PrimDecl(_, Location.Var(i, n)),
            psym
          ) <- params.zip(paramtypes)
        do
          if state.lookupLocal(n).isDefined
          then buffer += s"Variable (parameter) $n already declared" -> i
          else state += (n, psym)
        // set offset for the function body to use (-4 so decr gives correct start of -8)
        state.offset = -4
        // adds another enclosing scope so shadowing is allowed
        // TODO: change behavior if shadowing params is not allowed
        analyzeBlock(body, state)
      if bodyErrors.nonEmpty then buffer ++= bodyErrors
    // ========================= Tuple declaration =========================
    case TopLevel.TupDecl(Location.Var(i, name), fields) =>
      // TODO: get mapping of offsets from fields
      // scan left -> right tallying up the size of each field
      val local = Map[String, Sym]() // local field env
      val errs  = ListBuffer[(String, Int)]()
      import Location.Var
      // check for duplicate fields and type errors -- additionally keep track of offset + size information
      var offset = 0
      for field <- fields do
        val lb = ListBuffer[(String, Int)]()
        import VarDecl.*, PType.*
        val loc = field.name
        if local.contains(loc.name) then lb += s"field ${loc.name} already declared" -> loc.ind
        val (size, sym) = field match
          case PrimDecl(t, Var(i, n)) => // primitive field
            if t == Void then lb += s"field $n cant be of type void" -> i
            (4, VarSym(t, false, offset))
          case TupDecl(Var(ti, tn), _) => // tupled field
            if !state.hasTuple(tn) then lb += s"Tuple $tn not declared" -> ti
            val tup  = state.getTuple(tn)
            val size = tup.size
            (size, TupVarSym(tn, tup, false, offset, size))
        if lb.isEmpty then local += loc.name -> sym
        offset += size
      val sym = TupSym(local, offset)
      if state.hasTuple(name)
      then buffer += s"Tuple $name already declared" -> i
      else state.addTuple(name -> sym)
  buffer.toList

// called by toplevel declarations and local variable declarations
def processVar(
    vdecl: VarDecl,
    state: State,
    isGlobal: Boolean
): (Sym, ErrList) =
  val errs = ListBuffer[(String, Int)]()
  val (name, sym) = vdecl match
    case VarDecl.PrimDecl(ttype, l @ Location.Var(i, name)) =>
      vdecl.size = 4
      if ttype == PType.Void then errs += "Cannot declare variable of type void" -> i
      if state.lookupLocal(name).isDefined then errs += s"Variable $name already declared" -> i
      val offset = if isGlobal then 1 else state.nextVarOffset(4)
      (name, VarSym(ttype, isGlobal, offset))
    case VarDecl.TupDecl(Location.Var(ti, ttype), Location.Var(i, name)) =>
      if state.lookupLocal(name).isDefined then errs += s"Variable $name already declared" -> i
      if !state.hasTuple(ttype) then
        errs += s"Tuple $ttype not declared" -> ti
        null // dummy value
      else
        val tup = state.getTuple(ttype)
        val sz  = tup.size
        vdecl.size = sz
        val offset = if isGlobal then 1 else state.nextVarOffset(sz)
        (name, TupVarSym(ttype, tup, isGlobal, offset, sz))
  if errs.isEmpty then state += (name, sym)
  (sym, errs.toList)

// TODO: refactor out the common code between process and TupDecl's fields into some checkLocals function

def analyze(stmt: Stmt, state: State): ErrList =
  import Stmt.*
  stmt match
    case If(cond, thenStmt, elseStmt) => analyze(cond, state)
        ++ analyzeBlock(thenStmt, state)
        ++ elseStmt.fold(Nil)(analyzeBlock(_, state))
    case While(cond, body) => analyze(cond, state) ++ analyzeBlock(body, state)
    case Return(value)     => value.fold(Nil)(analyze(_, state))
    case ExprStmt(expr)    => analyze(expr, state)
    case Write(expr)       => analyze(expr, state)
    case Read(loc)         => analyzeLoc(loc, state)._1
    case Incr(loc)         => analyzeLoc(loc, state)._1
    case Decr(loc)         => analyzeLoc(loc, state)._1

def analyze(expr: Expr, state: State): ErrList =
  import Expr.*
  expr match
    case Call(_, name, args) => analyzeLoc(name, state)._1
        ++ args.flatMap(analyze(_, state))
    case l @ Loc(_, loc) =>
      val (errs, sym) = analyzeLoc(loc, state)
      if sym.isDefined then loc.sym = sym.get
      errs
    case UnOp(_, _, rhs)       => analyze(rhs, state)
    case BinOp(_, _, lhs, rhs) => analyze(lhs, state) ++ analyze(rhs, state)
    case Assign(_, lhs, rhs)   => analyze(Loc(-1, lhs), state) ++ analyze(rhs, state)
    case _                     => Nil

// errors and linked symbol information information
def analyzeLoc(
    lval: Location,
    state: State
): (ErrList, Option[Sym]) = lval match
  case Location.Var(ind, name) => state.lookupGlobal(name) match
      case Some(sym) =>
        lval.sym = sym
        (Nil, Some(sym))
      case None => (Seq(s"Variable $name not declared" -> ind), None)
  case Location.Tuple(_, lhs, Location.Var(ind, name)) =>
    // use analyzeLoc to get the offset associated with the left hand side
    // use the RHS to get a more specific offset
    // IDFK how to do this bruh D=
    // TODO: apparently i dont even use the offset here
    analyzeLoc(lhs, state) match
      case (Nil, Some(TupVarSym(tt, tup, isGlob, offBase, size))) =>
        tup.fields.get(name) match
          case Some(sym) => 
            val nsym = sym match
                case VarSym(ttype, _, off, size) =>
                  VarSym(ttype, isGlob, offBase + off, size)
                case TupVarSym(ttype, tup, _, off, size) =>
                  TupVarSym(ttype, tup, isGlob, offBase + off, size)
                case _ => error(s"internal err - illegal tuple field type $sym")
            lval.sym = nsym
            (
              Nil,
              Some(nsym)
            )
          case None =>
            (Seq(s"Field $name not declared in tuple $tt" -> ind), None)
      case (Nil, Some(VarSym(t, _, _, _))) =>
        (Seq(s"attempt to index $t" -> ind), None)
      case (err, _) => (err, None)
  // ???

def analyzeBlock(body: Body, state: State): ErrList =
  state.enclose:
    body._1.foldLeft(Nil)(_ ++ processVar(_, state, false)._2)
      ++ body._2.foldLeft(Nil)(_ ++ analyze(_, state))
