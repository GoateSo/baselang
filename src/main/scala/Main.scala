import fastparse.*
import baselang.*
import fastparse.Parsed.Success
import fastparse.Parsed.Failure

val wd = os.pwd

def formatErr(err: (String, Int), program: String): String =
  val (r, c) = lineCol(err._2, program)
  s"$r:$c\t****ERROR**** ${err._1}"

@main def hello(): Unit =
  var cur = ""
  println("Enter a file to compile (or 'exit' to quit):")
  while
    cur = scala.io.StdIn.readLine("base> ")
    cur != "exit"
  do
    if cur.endsWith(".base") then
      val file = os.read(wd / cur)
      fastparse.parse(file, program(_)) match
        case Success(value, index) =>
          println("parsing successful")
          val (errs, state) = analyze(value)
          if errs.isEmpty then
            println("No errors found")
            val errs2 = typecheck(value, state)
            if errs2.isEmpty then
              println("Typechecking successful")
              os.write(wd / s"${cur.dropRight(5)}.s", codeGen(value))
              println(s"wrote generated assembly code to ${cur.dropRight(5)}.s")
            else errs2.map(formatErr(_, file)).foreach(println)
          else errs.map(formatErr(_, file)).foreach(println)
        case f: Failure =>
          println(s"Error parsing file: ${f.extra.traced.trace}")
    else println("file/command not supported")
