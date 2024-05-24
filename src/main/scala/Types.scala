package baselang
import scala.collection.mutable.Map

type Err     = (String, Int)
type ErrList = Seq[Err]

def error(msg: String) = throw new Exception(msg)

sealed trait Sym(val isGlobal: Boolean = true, val offset: Int = Integer.MIN_VALUE)
case class VarSym(ttype: PType, info: (Boolean, Int)) extends Sym(info._1, info._2)
case class TupVarSym(ttype: String, tup: TupSym, info: (Boolean, Int))
    extends Sym(info._1, info._2)
case class FunSym(retType: PType, params: Seq[VarSym]) extends Sym
case class TupSym(fields: Map[String, Sym], val size: Int) extends Sym

enum VType:
  case Error, Void, Integer, Logical, Str
  case Fun(retType: VType, params: Seq[VType])
  case Tup(name: String)

type ExpRes = (VType, ErrList)

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
      case VarSym(ttype, _)       => ttype.toVType
      case TupVarSym(ttype, _, _) => Tup(ttype)
      case FunSym(ret, params)    => Fun(ret.toVType, params.map(_.toVType))
      case TupSym(fields, _)      => Error
      case null                   => Error
