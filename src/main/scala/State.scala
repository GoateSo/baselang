package baselang
import scala.collection.mutable.{Map, ListBuffer}

/** State class for mutable state during name analysis
  *
  * @param vars
  *   symbol table containing variable names and their symbols
  * @param tuples
  *   a collection of top level tuple (type) declarations
  * @param offset
  *   current offset for local variables (for use during codegen)
  */
final class State(
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
    vars.findLast(_.contains(name)).map(_(name))
  def hasConflict(name: String): Boolean    = vars.last.contains(name)
  def hasTuple(name: String): Boolean       = tuples.contains(name)
  def getTuple(name: String): TupSym        = tuples(name)
  def addTuple(tup: (String, TupSym)): Unit = tuples += tup
  def nextVarOffset(incr: Int): Int =
    val ret = offset
    offset -= incr
    ret
  def clear(): Unit =
    vars.clear()
    vars += Map() // global scope
    tuples.clear()
    offset = 0
end State
