package baselang;
import scala.collection.mutable.{ListBuffer, Map}

sealed trait Sym(val isGlobal: Boolean = false, var offset: Int = 0)
case class VarSym(ttype: PType, glob: Boolean, off: Int) extends Sym(glob, off)
case class TupVarSym(ttype: String) extends Sym
case class FunSym(retType: PType, params: Seq[VarSym]) extends Sym
case class TupSym(fields: Map[String, Sym]) extends Sym

case class State(
    var vars: ListBuffer[Map[String, Sym]] = ListBuffer(Map()),
    var tuples: Map[String, TupSym] = Map(),
    var offset: Int = 0
):
  def pushScope(): Unit = vars += Map()
  def popScope(): Unit = vars.dropRightInPlace(1)
  def enclose[T](body: => T): T =
    pushScope()
    val res = body
    popScope()
    res
  def +=(name: String, sym: Sym): Unit = vars.last += (name -> sym)
  def lookupGlobal(name: String): Option[Sym] =
    vars.findLast(_.contains(name)).flatMap(_.get(name))
  def lookupLocal(name: String): Option[Sym] = vars.last.get(name)
  def hasTuple(name: String): Boolean = tuples.contains(name)
  def getTuple(name: String): TupSym = tuples(name)
  def addTuple(tup: (String, TupSym)): Unit = tuples += tup
  def nextVarOffset(): Int =
    offset -= 4
    offset

def analyze(prog: Seq[TopLevel]): (Seq[(String, Int)], State) =
  val state: State = State()
  val res = prog.foldLeft(Nil)(_ ++ analyze(_, state))
  if state.lookupGlobal("main").isEmpty then
    (res :+ "No main function declared" -> 0, state)
  else (res, state)

def analyze(tdecl: TopLevel, state: State): Seq[(String, Int)] =
  processDecl(tdecl, state)._2

// called by toplevel declarations and local variable declarations
def process(
    vdecl: VarDecl,
    state: State,
    isGlobal: Boolean
): (Sym, Seq[(String, Int)]) =
  val errs = ListBuffer[((String, Int))]()
  val (name, sym) = vdecl match
    case VarDecl.VDecl(ttype, l @ Location.Var(i, name)) =>
      if ttype == PType.Void then
        errs += "Cannot declare variable of type void" -> i
      if state.lookupLocal(name).isDefined then
        errs += s"Variable $name already declared" -> i
      val offset = if isGlobal then 1 else state.nextVarOffset()
      (name, VarSym(ttype, isGlobal, offset))
    case VarDecl.TupVDecl(Location.Var(ti, ttype), Location.Var(i, name)) =>
      if !state.hasTuple(ttype) then errs += s"Tuple $ttype not declared" -> ti
      if state.lookupLocal(name).isDefined then
        errs += s"Variable $name already declared" -> i
      (name, TupVarSym(ttype))
  if errs.isEmpty then state += (name, sym)
  (sym, errs.toList)

def analyze(stmt: Stmt, state: State): Seq[(String, Int)] =
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

def analyze(expr: Expr, state: State): Seq[(String, Int)] =
  import Expr.*
  expr match
    case Call(_, name, args) => analyzeLoc(name, state)._1
        ++ args.flatMap(analyze(_, state))
    case Loc(_, loc)           => analyzeLoc(loc, state)._1
    case UnOp(_, _, rhs)       => analyze(rhs, state)
    case BinOp(_, _, lhs, rhs) => analyze(lhs, state) ++ analyze(rhs, state)
    case Assign(_, lhs, rhs) => analyzeLoc(lhs, state)._1 ++ analyze(rhs, state)
    case _                   => Nil

// errors and linked symbol information information
def analyzeLoc(
    lval: Location,
    state: State
): (Seq[(String, Int)], Option[Sym]) = lval match
  case Location.Var(ind, name) => state.lookupGlobal(name) match
      case Some(sym) =>
        lval.sym = sym
        (Nil, Some(sym))
      case None => (Seq(s"Variable $name not declared" -> ind), None)
  case Location.Tuple(_, lhs, Location.Var(ind, name)) =>
    analyzeLoc(lhs, state) match
      // TODO maybe check whether tuples(tt) exists
      case (Nil, Some(TupVarSym(tt))) =>
        state.getTuple(tt).fields.get(name) match
          case Some(sym) =>
            lval.sym = sym
            (Nil, Some(sym))
          case None =>
            (Seq(s"Field $name not declared in tuple $tt" -> ind), None)
      case (Nil, Some(VarSym(t, _, _))) =>
        (Seq(s"attempt to index $t" -> ind), None)
      case (err, _) => (err, None)

def analyzeBlock(body: Body, state: State): Seq[(String, Int)] =
  state.enclose:
    body._1.foldLeft(Nil)(_ ++ process(_, state, false)._2)
      ++ body._2.foldLeft(Nil)(_ ++ analyze(_, state))

def processDecl(decl: TopLevel, state: State): (Sym, Seq[(String, Int)]) =
  val buffer = ListBuffer[(String, Int)]()
  val sym = decl match
    // ====================== variable declaration ======================
    case TopLevel.VDecl(vdec) =>
      val (s, errs) = process(vdec, state, true)
      buffer ++= errs; s
    // ====================== function declaration ========================
    case TopLevel.FunDecl(retType, name, params, body) =>
      // check for unique name
      var badDefn = false
      val str = name.name
      if state.lookupGlobal(str).isDefined then
        buffer += s"name '$name' already in use" -> name.ind
        badDefn = true
      // get parameter types/offsets and push function signature (if no name conflict)
      val paramtypes =
        params.zipWithIndex.map((x, i) => VarSym(x.ttype, false, 4 + 4 * i))
      val nsym = FunSym(retType, paramtypes)
      name.sym = nsym
      // push function signature into enclosing scope
      if !badDefn then state += (str, FunSym(retType, paramtypes))
      val bodyErrors = state.enclose:
        // check that parameters are unique and push into enclosed scope
        for case (
            VarDecl.VDecl(_, Location.Var(i, n)),
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
      FunSym(retType, paramtypes)
    // ========================= Tuple declaration =========================
    case TopLevel.TupDecl(Location.Var(i, name), fields) =>
      val local = Map[String, Sym]() // local field env
      import Location.Var
      for field <- fields do
        val lb = ListBuffer[(String, Int)]()
        val mapping = field match
          case VarDecl.VDecl(t, Var(i, n)) =>
            if t == PType.Void then lb += s"field $n cant be of type void" -> i
            if local.contains(n) then lb += s"field $n already declared" -> i
            (n -> VarSym(t, false, -1))
          // TODO: find proper offset when implementing tuple
          case VarDecl.TupVDecl(Var(ti, t), Var(i, n)) =>
            if !state.hasTuple(t) then lb += s"Tuple $t not declared" -> ti
            if local.contains(n) then lb += s"field $n already declared" -> i
            (n -> TupVarSym(t))
        if lb.nonEmpty then buffer ++= lb else local += mapping
      val sym = TupSym(local)
      if state.hasTuple(name)
      then buffer += s"Tuple $name already declared" -> i
      else state.addTuple(name -> sym)
      sym
  (sym, buffer.toList)
