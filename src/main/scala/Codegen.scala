package baselang
import scala.collection.mutable.Map
var labelCounter = 0
// map of strings to labels
var interned = Map[String, String]()
// TODO: figure out local variable offsets from vardecls (do so in name analysis)
// convert code to MIPS assembly
def codeGen(program: Seq[TopLevel]): String =
  labelCounter = 0
  interned = Map()
  program.map(codeGen).mkString("\n")

def codeGen(tdecl: TopLevel): String =
  import TopLevel.*
  tdecl match
    case FunDecl(ret, name, ps, bdy) => genFundecl(ret, name, ps, bdy)
    case VDecl(vdecl)                => genGlobal(vdecl)
    case TupDecl(name, fields)       => ???

def genGlobal(vdecl: VarDecl): String =
  import VarDecl.*
  vdecl match
    case VDecl(pt, vd) =>
      s"""|${"\t"}.data
          |${"\t"}.align 2
          |_${vd.name}:${"\t"}.space 4""".stripMargin
    case TupVDecl(tt, td) => ???

def genFundecl(
    retType: PType,
    name: Location.Var,
    params: Seq[VarDecl.VDecl],
    body: Body
): String =
  val fname = name.name
  combine(
    // preamble
    "\t.text",
    if (fname == "main") then s"\t.globl main\nmain:"
    else s"\t_${fname}:",
    // prologue
    genPush(RA),
    genPush(FP),
    genInst("addu", FP, SP, "8"),
    if (body._1.nonEmpty) then
      genWithComment(
        "subu",
        "allocating space for locals",
        SP,
        SP,
        s"${body._1.length * 4}"
      )
    else "",
    // body
    codeGen(body, fname),
    // epilogue
    genLabel(s"${fname}_Exit"),
    genIndexed("lw", RA, FP, 0, s"load return addr for $fname"),
    genWithComment("move", "old sp", T0, FP),
    genIndexed("lw", FP, FP, -4, s"restore old fp for $fname"),
    genWithComment("move", "restore sp", SP, T0),
    if (fname != "main")
      genWithComment("jr", "return", RA)
    else
      combine(
        genWithComment("li", "exit", V0, "10"),
        genWithComment("syscall", "exit")
      )
  )

def codeGen(b: Body, fname: String): String =
  val (vdecls, stmts) = b
  stmts.map(codeGen(_, fname)).mkString("\n")

private inline def incrStmt(inline loc: Location, inline amt: Int) =
  combine(
    loadAddr(loc),
    genPop(T0),
    genIndexed("lw", T1, T0, 0),
    genInst("addi", T1, T1, s"$amt"),
    genIndexed("sw", T1, T0, 0)
  )

def codeGen(stmt: Stmt, fname: String): String =
  import Stmt.*
  val res = stmt match
    case If(cond, thenStmt, elseStmt) =>
      val l1 = nextLabel()
      // common code for both (evaluate condition and set up then case)
      val temp = combine(
        codeGen(cond),
        genPop(T0),
        genInst("li", T1, "0"),
        genWithComment("beq", "if jump", T0, T1, l1),
        codeGen(thenStmt, fname)
      )
      // add on else case if it exists
      val rest = elseStmt match
        case None => genLabel(l1) // use l1 as end label
        case Some(x) =>
          val endLabel = nextLabel() // use l1 as else label
          combine(
            genInst("j", endLabel),
            genLabel(l1),
            codeGen(x, fname),
            genLabel(endLabel)
          )
      combine(temp, rest)
    case While(cond, body) =>
      val lstart = nextLabel()
      val lend = nextLabel()
      // eval condition
      combine(
        genLabel(lstart, "while loop start"),
        codeGen(cond),
        genPop(T0),
        // jmp if false
        genInst("li", T1, "0"),
        genInst("beq", T0, T1, lend),
        // body
        codeGen(body, fname),
        // jmp back to start
        genInst("j", lstart),
        genLabel(lend, "while loop end label")
      )
    case Return(value) =>
      val getRetVal =
        if value.isDefined
        then combine(codeGen(value.get), genPop(V0))
        else ""
      combine(getRetVal, genInst("j", s"${fname}_Exit"))
    case ExprStmt(expr) => combine(codeGen(expr), genPop(T0))
    case Write(expr) =>
      val code = if expr.myType == VType.Str then "4" else "1"
      combine(
        codeGen(expr),
        genPop(A0),
        genInst("li", V0, code),
        genInst("syscall")
      )
    case Read(loc) => combine(
        genInst("li", V0, "5"),
        genInst("syscall"),
        loadAddr(loc),
        genPop(T0),
        genIndexed("sw", V0, T0, 0)
      )
    case Incr(loc) => incrStmt(loc, 1)
    case Decr(loc) => incrStmt(loc, -1)
  res
  // combine(comment(s"${mkString(stmt, 0).dropRight(2)}"), res)
def codeGen(expr: Expr): String =
  import Expr.*
  expr match
    case IntLit(i, value)  => combine(genInst("li", T0, s"$value"), genPush(T0))
    case LogiLit(i, value) => codeGen(IntLit(i, if value then 1 else 0))
    case StringLit(i, value)    => genStr(value)
    case Loc(i, loc)            => codeGen(loc)
    case Call(i, name, args)    => genCall(name.name, args)
    case UnOp(i, op, rhs)       => genUnop(op, codeGen(rhs))
    case BinOp(i, op, lhs, rhs) => genBinaryOp(op, codeGen(lhs), rhs)
    case Assign(i, lhs, rhs)    => genAssign(lhs, rhs)

def genUnop(op: UnaryOp, rhs: String): String =
  val opStr = op match
    case UnaryOp.Neg =>
      combine(genInst("li", T1, "0"), genInst("subu", T0, T1, T0))
    case UnaryOp.Not =>
      combine(genInst("li", T1, "1"), genInst("subu", T0, T1, T0))
  combine(rhs, genPop(T0), opStr)

def genBinaryOp(op: BinaryOp, lhs: String, rhs: Expr): String =
  import BinaryOp.*
  val eagerOp: Map[BinaryOp, String] = Map( // map for eager operations
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
  val shortVal = Map(And -> "0", Or -> "1")
  op match
    case Add | Sub | Mul | Div | Eq | Neq | Lt | Gt | Le | Ge =>
      combine(
        lhs,
        codeGen(rhs),
        genPop(T1),
        genPop(T0),
        eagerOp(op),
        genPush(T0)
      )
    case And | Or => // short circuit operations
      val lab = nextLabel()
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
import BinaryOp.*

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
def codeGen(lval: Location): String =
  import Location.*
  lval match
    case Var(i, name) =>
      combine(
        if lval.sym.isGlobal then genInst("lw", T0, s"_$name")
        else genIndexed("lw", T0, FP, lval.sym.offset, s"load value of $name"),
        genPush(T0)
      )
    case Tuple(i, lhs, rhs) =>
      throw Exception("attempted to get value associated with tuple")
// get associated memory address
def loadAddr(lval: Location): String =
  import Location.*
  lval match
    case Var(i, name) =>
      val sym = lval.sym
      combine(
        if sym.isGlobal
        then genInst("la", T0, s"_$name")
        else genIndexed("la", T0, FP, sym.offset),
        genPush(T0)
      ) // TODO: check this works
    case Tuple(i, lhs, rhs) => ???

inline def genCall(inline name: String, inline args: Seq[Expr]): String =
  // precall
  val a1 = args.reverse.map(codeGen).mkString("\n")
  val jal = genInst("jal", s"_$name") // gen JAL
  // post call
  val mov = genInst("add", SP, SP, s"${args.length * 4}")
  val a2 = genPush(V0)
  combine(a1, jal, mov, a2)

inline def genStr(value: String): String =
  val str =
    if !interned.contains(value)
    then
      val lab = nextLabel()
      interned(value) = lab
      combine(
        "\t.data",
        lab + ":\t.asciiz " + s"\"$value\"",
        "\t.text"
      )
    else ""
  combine(
    str,
    genWithComment(
      "la",
      "load address of string literal",
      T0,
      interned(value)
    ),
    genPush(T0)
  )

// general helper functions
inline def genPush(inline value: String): String =
  combine(
    genIndexed("sw", value, SP, 0, s"PUSH $value"),
    genInst("subu", SP, SP, "4")
  )

inline def genPop(inline value: String): String =
  combine(
    genIndexed("lw", value, SP, 4, s"POP to $value"),
    genInst("addu", SP, SP, "4")
  )

final inline val MAXLEN = 4

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
inline def combine(inline ops: String*): String = ops.filter(_.nonEmpty).mkString("\n")
// useful registers
inline val SP = "$sp"
inline val FP = "$fp"
inline val RA = "$ra"
inline val A0 = "$a0"
inline val V0 = "$v0"
inline val T0 = "$t0"
inline val T1 = "$t1"
