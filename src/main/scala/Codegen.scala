package baselang
import scala.collection.mutable.Map

object Codegen:
  var labelCounter = 0
  import Location.*, TopLevel.*, VarDecl.*
  // map of strings to labels
  val interned = Map[String, String]()

  /** Generates MIPS assembly code for a program
    *
    * @param program
    *   parsed AST for program
    * @return
    *   string representation of the generated assembly code
    */
  def apply(program: Seq[TopLevel]): String =
    interned.clear()
    program.map(codeGen).filter(_ != "").mkString("\n")

  def codeGen(tdecl: TopLevel): String = tdecl match
    case FunDecl(ret, name, ps, bdy) => genFundecl(ret, name, ps, bdy)
    case VarDec(vdecl)               => genGlobal(vdecl)
    case TupDecl(name, fields)       => "" // tuple decls are basically just typedefs

  def genGlobal(vdecl: VarDecl): String =
    s"""|${"\t"}.data
        |${"\t"}.align 2
        |_${vdecl.name.name}:${"\t"}.space ${vdecl.size}""".stripMargin

  def genFundecl(ret: PType, name: Var, params: Seq[PrimDecl], body: Body): String =
    val fname     = name.name
    val decls     = body._1
    val localSize = decls.map(_.size).sum
    val preamble = combine(
      "\t.text",
      if fname == "main"
      then s"\t.globl main\nmain:"
      else s"\t_${fname}:"
    )
    // prologue: save RA and FP, move FP, and allocate space for locals
    val prologue = combine(
      genPush(RA),
      genPush(FP),
      genInst("addu", FP, SP, "8"),
      if decls.nonEmpty
      then genInst("subu", SP, SP, s"$localSize")
      else ""
    )
    // restore old RA, FP, and SP; jump back to caller
    val epilogue = combine(
      genLabel(s"${fname}_Exit"),
      genIndexed("lw", RA, FP, 0, s"load return addr for $fname"),
      genWithComment("move", "old sp", T0, FP),
      genIndexed("lw", FP, FP, -4, s"restore old fp for $fname"),
      genWithComment("move", "restore sp", SP, T0),
      if fname == "main" then
        combine(
          genWithComment("li", "exit", V0, "10"),
          genWithComment("syscall", "exit")
        )
      else genWithComment("jr", "return", RA)
    )
    // put it all together
    combine(preamble, prologue, codeGen(body, fname), epilogue)

  def codeGen(b: Body, fname: String): String =
    val (vdecls, stmts) = b
    stmts.map(codeGen(_, fname)).mkString("\n")

  private def incrStmt(loc: Location, amt: Int) = combine(
    loadAddr(loc),
    genPop(T0),
    genIndexed("lw", T1, T0, 0),
    genInst("addi", T1, T1, s"$amt"),
    genIndexed("sw", T1, T0, 0)
  )

  import Stmt.*
  private def genWhile(body: Body, cond: Expr, fname: String): String =
    val lstart = nextLabel()
    val lend   = nextLabel()
    combine(
      genLabel(lstart, "while loop start"),
      codeGen(cond),
      genPop(T0),
      genInst("li", T1, "0"),
      genInst("beq", T0, T1, lend),
      codeGen(body, fname),
      genInst("j", lstart),
      genLabel(lend, "while loop end label")
    )
  private def genRetVal(value: Option[Expr]): String =
    // if not void, evaluate expression and put into return register, otherwise no-op
    value.fold("")(x => combine(codeGen(x), genPop(V0)))

  private def genWrite(expr: Expr): String = combine(
    codeGen(expr),
    genPop(A0),
    genInst("li", V0, if expr.myType == VType.Str then "4" else "1"),
    genInst("syscall")
  )

  private def genRead(loc: Location): String = combine(
    genInst("li", V0, "5"),
    genInst("syscall"),
    loadAddr(loc),
    genPop(T0),
    genIndexed("sw", V0, T0, 0)
  )
  def codeGen(stmt: Stmt, fname: String): String = stmt match
    case If(cond, thenStmt, elseStmt) =>
      val l1 = nextLabel()
      // common code for both (evaluate condition and set up then case)
      val thenCase = combine(
        codeGen(cond),
        genPop(T0),
        genInst("li", T1, "0"),
        genWithComment("beq", "if jump", T0, T1, l1),
        codeGen(thenStmt, fname)
      )
      // add on else case if it exists
      val rest = elseStmt.fold(genLabel(l1)): x =>
        val endLabel = nextLabel()
        combine(
          genInst("j", endLabel),
          genLabel(l1),
          codeGen(x, fname),
          genLabel(endLabel)
        )
      combine(thenCase, rest)
    case While(cond, body) => genWhile(body, cond, fname)
    case Return(value)     => combine(genRetVal(value), genInst("j", s"${fname}_Exit"))
    case ExprStmt(expr)    => combine(codeGen(expr), genPop(T0))
    case Write(expr)       => genWrite(expr)
    case Read(loc)         => genRead(loc)
    case Incr(loc)         => incrStmt(loc, 1)
    case Decr(loc)         => incrStmt(loc, -1)

  import Expr.*
  def codeGen(expr: Expr): String = expr match
    case IntLit(i, value)    => combine(genInst("li", T0, s"$value"), genPush(T0))
    case LogiLit(i, value)   => codeGen(IntLit(i, if value then 1 else 0))
    case StringLit(i, value) => genStr(value)
    case Loc(loc)            => codeGen(loc)
    case Call(name, args)    => genCall(name.name, args)
    case UnOp(op, rhs)       => genUnop(op, codeGen(rhs))
    case BinOp(op, lhs, rhs) => genBinaryOp(op, codeGen(lhs), rhs)
    case Assign(lhs, rhs)    => genAssign(lhs, rhs)

  import BinaryOp.*, UnaryOp.*
  def genUnop(op: UnaryOp, rhs: String): String =
    val opStr = op match
      case Neg => "0" // -x = 0-x
      case Not => "1" // !x = 1-x
    combine(
      rhs,
      genPop(T0),
      genInst("li", T1, opStr),
      genInst("subu", T0, T1, T0),
      genPush(T0)
    )

  private def genEager(op: String, lhs: String, rhs: String): String = combine(
    lhs,
    rhs,
    genPop(T1),
    genPop(T0),
    op,
    genPush(T0)
  )

  private def genShortCircuit(op: BinaryOp, lhs: String, rhs: Expr): String =
    val shortVal = Map(And -> "0", Or -> "1") // exit if false for and + exit if true for or
    val lab      = nextLabel()
    combine(
      lhs, // eval lhs case
      genPop(T0),
      genInst("li", T1, shortVal(op)), // short circuit check
      genInst("beq", T0, T1, lab),
      codeGen(rhs), // push eval rhs caes
      genPop(T0),
      genLabel(lab), // short circuit label
      genPush(T0)
    )

  private val eagerOp: Map[BinaryOp, String] = Map( // map for eager operations
    Add -> genInst("add", T0, T0, T1),
    Sub -> genInst("sub", T0, T0, T1),
    Mul -> combine(genInst("mult", T0, T1), genInst("mflo", T0)),
    Div -> combine(genInst("div", T0, T1), genInst("mflo", T0)),
    Eq -> genInst("seq", T0, T0, T1),
    Neq -> genInst("sne", T0, T0, T1),
    Lt -> genInst("slt", T0, T0, T1),
    Gt -> genInst("sgt", T0, T0, T1),
    Le -> genInst("sle", T0, T0, T1),
    Ge -> genInst("sge", T0, T0, T1)
  )

  def genBinaryOp(op: BinaryOp, lhs: String, rhs: Expr): String = op match
    case And | Or => genShortCircuit(op, lhs, rhs)
    case _        => genEager(eagerOp(op), lhs, codeGen(rhs))

  def genAssign(lval: Location, rval: Expr): String = combine(
    codeGen(rval), // codegen into T1
    loadAddr(lval), // load to T0
    genPop(T0),
    genPop(T1),
    // store T1 into T0 (RHS into LHS)
    genIndexed("sw", T1, T0, 0),
    genPush(T1) // push RHS back onto stack for future use
  )

  // get associated value
  def codeGen(lval: Location): String = lval match
    case Var(i, name) => combine(
        if lval.sym.isGlobal then genInst("lw", T0, s"_$name")
        else genIndexed("lw", T0, FP, lval.sym.offset, s"load value of $name"),
        genPush(T0)
      )
    case Tuple(i, lhs, rhs) => combine(
        loadAddr(lval), // load address of tuple into memory
        genPop(T0), // pop address into T0
        // load value at the indexed location
        genIndexed("lw", T0, T0, 0),
        genPush(T0)
      )

  // get root of a tuple index (furthest left term)
  private def getRoot(lval: Location): Var = lval match
    case l: Var         => l
    case Tuple(_, l, _) => getRoot(l)

  // get offset of a tuple indexing operation
  private def getOffset(lval: Location): Int = lval match
    case Var(i, name) => 0
    case Tuple(i, lhs, rhs) =>
      val leftOffset = getOffset(lhs)
      lhs.sym match
        case TupVarSym(ttype, tup, _) =>
          val right       = tup.fields(rhs.name)
          val localOffset = right.offset
          leftOffset + localOffset
        case _ => error(s"internal err - illegal tuple var type ${lhs.sym}")

  // get associated memory address
  def loadAddr(lval: Location): String = lval match
    case Var(i, name) =>
      val sym = lval.sym
      combine(
        if sym.isGlobal
        then genInst("la", T0, s"_$name")
        else genIndexed("la", T0, FP, sym.offset),
        genPush(T0)
      ) // TODO: check this works
    case Tuple(i, lhs, rhs) =>
      val root    = getRoot(lval)
      val rootSym = root.sym
      // T0 should contain the indexed into the tuple
      combine(
        if rootSym.isGlobal // load tuple address into memory
        then genInst("la", T0, s"_${root.name}")
        else genIndexed("la", T0, FP, rootSym.offset, s"allocate tuple index ${Unparse(lval)}"),
        // find and then add the correct offset
        genInst("addi", T0, T0, s"-${getOffset(lval)}"),
        genPush(T0)
      )

  inline def genCall(inline name: String, inline args: Seq[Expr]): String =
    // precall (note: args pushed in reverse order)
    val a1  = args.reverse.map(codeGen).mkString("\n")
    val jal = genInst("jal", s"_$name") // gen JAL
    // post call (note: this only works since parameters are of size 4; change if tuple params are allowed)
    val mov = genInst("add", SP, SP, s"${args.length * 4}")
    val a2  = genPush(V0)
    combine(a1, jal, mov, a2)

  inline def genStr(value: String): String =
    val strLit =
      if !interned.contains(value)
      then
        val lab = nextLabel()
        interned(value) = lab
        "\t.data\n"
          ++ lab + ":\t.asciiz " + s"\"$value\"\n"
          ++ "\t.text"
      else ""
    combine(
      strLit,
      genWithComment("la", "load strlit addr", T0, interned(value)),
      genPush(T0)
    )

  // general helpers
  final inline val MAXLEN = 4

  inline def genPush(inline value: String): String = combine(
    genIndexed("sw", value, SP, 0, s"PUSH $value"),
    genInst("subu", SP, SP, "4")
  )

  inline def genPop(inline value: String): String = combine(
    genIndexed("lw", value, SP, 4, s"POP to $value"),
    genInst("addu", SP, SP, "4")
  )

  inline def genIndexed(
      inline op: String,
      inline a1: String,
      inline a2: String,
      inline ind: Int,
      inline comment: String = ""
  ): String =
    val space = " " * (MAXLEN - op.length + 2)
    s"\t$op$space$a1, $ind($a2)\t\t# $comment"

  private inline def nextLabel(): String =
    labelCounter += 1
    s".L$labelCounter"

  inline def genLabel(inline lab: String, inline cmt: String = ""): String =
    s"$lab:\t\t${if cmt != "" then "# " + cmt else ""}"

  inline def genWithComment(
      inline op: String,
      inline com: String,
      inline ops: String*
  ): String =
    val space = " " * (MAXLEN - op.length + 2)
    s"\t$op$space${ops.mkString(", ")}\t\t# ${com}"

  inline def genInst(inline op: String, ops: String*): String =
    val space = " " * (MAXLEN - op.length + 2)
    s"\t$op$space${ops.mkString(", ")}"

  inline def combine(inline op: String, inline op2: String): String = s"$op\n$op2"
  inline def combine(inline ops: String*): String =
    ops.filter(_.nonEmpty).mkString("\n")
  // useful registers
  inline val SP = "$sp"
  inline val FP = "$fp"
  inline val RA = "$ra"
  inline val A0 = "$a0"
  inline val V0 = "$v0"
  inline val T0 = "$t0"
  inline val T1 = "$t1"
