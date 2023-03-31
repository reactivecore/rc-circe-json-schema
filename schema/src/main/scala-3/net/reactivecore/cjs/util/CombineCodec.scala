package net.reactivecore.cjs.util

import io.circe.Decoder.Result
import io.circe.{Codec, HCursor, JsonObject}
import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline}
import io.circe.Codec.AsObject
import io.circe.Json
import cats.implicits._

trait CombineCodec[T] {
  def codec: Codec.AsObject[T]
}

object CombineCodec {

  implicit inline def genericCodec[T <: Product](using m: Mirror.ProductOf[T]): CombineCodec[T] = {
    val tupleVariant   = genericTuple[m.MirroredElemTypes]
    val tupleConverter = listToTuple[m.MirroredElemTypes]
    val c = new Codec.AsObject[T] {
      def encodeObject(a: T): JsonObject = {
        JsonObject.fromIterable ({
          for {
            (v, codec) <- a.productIterator.zip(tupleVariant)
            (key, jsonValue) <- codec.encodeObject(v).toIterable.iterator
          } yield (key, jsonValue)
        }.toSeq)
      }

      def apply(c: HCursor): Result[T] = {
        val parts = tupleVariant.map { codec =>
          codec.apply(c)
        }
        parts.sequence.map { decoded =>
          m.fromTuple(
            tupleConverter(decoded)
          )
        }
      }
    }
    new CombineCodec[T] {
      def codec: AsObject[T] = c
    }
  }

  private inline def genericTuple[T <: Tuple]: List[Codec.AsObject[Any]] = {
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts) =>
        val first = summonInline[Codec.AsObject[t]].asInstanceOf[Codec.AsObject[Any]]
        val last = genericTuple[ts]
        first :: last
    }
  }

  private inline def listToTuple[T <: Tuple]: List[Any] => T = {
    inline erasedValue[T] match {
      case _: EmptyTuple => _ => EmptyTuple.asInstanceOf[T]
      case _: (t *: ts) =>
        list =>
          (list.head.asInstanceOf[t] *: listToTuple[ts](list.tail)).asInstanceOf[T]
    }
  }
}
