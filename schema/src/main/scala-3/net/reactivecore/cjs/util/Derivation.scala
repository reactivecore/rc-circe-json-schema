package net.reactivecore.cjs.util
import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.Mirror

object Derivation {
  
  inline def deriveLabels[T](using m: Mirror.Of[T]): List[String] = {
      // Also See https://stackoverflow.com/a/70416544/335385
      summonLabels[m.MirroredElemLabels]
  }

  inline def summonLabels[T <: Tuple]: List[String] = {
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[ValueOf[t]].value.asInstanceOf[String] :: summonLabels[ts]
    }
  }

}
