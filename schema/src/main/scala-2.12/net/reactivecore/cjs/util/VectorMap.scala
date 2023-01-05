package net.reactivecore.cjs.util

import io.circe._

import scala.collection.generic.{CanBuildFrom, MapFactory}
import scala.collection.immutable.MapLike
import scala.collection.{GenTraversableOnce, mutable}

/** Encodes a Map into a vector of keys and values */
case class VectorMap[K, +T](
    underlying: Vector[(K, T)]
) extends Map[K, T]
    with MapLike[K, T, VectorMap[K, T]] {

  override def +[V1 >: T](kv: (K, V1)): VectorMap[K, V1] = {
    underlying.indexWhere(_._1 == kv._1) match {
      case -1 => VectorMap(underlying :+ (kv))
      case n  => VectorMap(underlying.updated(n, kv))
    }
  }

  override def size: Int = underlying.size

  override def ++[V1 >: T](xs: GenTraversableOnce[(K, V1)]): VectorMap[K, V1] = {
    if (xs.isEmpty) {
      this
    } else if (this.isEmpty) {
      val got = mutable.Set.empty[K]
      val builder = Vector.newBuilder[(K, V1)]
      xs.foreach { case (k, v) =>
        if (got.contains(k)) {
          // skip
        } else {
          got += k
          builder += ((k, v))
        }
      }
      VectorMap(builder.result())
    } else {
      super.++(xs)
    }
  }

  override def get(key: K): Option[T] = underlying.collectFirst {
    case (k, value) if k == key => value
  }

  override def iterator: Iterator[(K, T)] = underlying.iterator

  override def -(key: K): VectorMap[K, T] = VectorMap(
    underlying.filterNot(_._1 == key)
  )

  override def empty: VectorMap[K, T] = VectorMap.empty

  override def toVector: Vector[(K, T)] = underlying
}

object VectorMap extends MapFactory[VectorMap] {

  override def empty[A, B]: VectorMap[A, B] = VectorMap(Vector.empty)

  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), VectorMap[A, B]] =
    ReusableCBF.asInstanceOf[CanBuildFrom[Coll, (A, B), VectorMap[A, B]]]
  private[this] val ReusableCBF = new MapCanBuildFrom[Any, Any]

  implicit def encoder[K, T](implicit k: KeyEncoder[K], e: Encoder[T]): Encoder.AsObject[VectorMap[K, T]] =
    Encoder.AsObject.instance { value =>
      JsonObject(
        value.underlying.map { case (key, value) =>
          k(key) -> e(value)
        }: _*
      )
    }

  implicit def decoder[K, T](implicit k: KeyDecoder[K], d: Decoder[T]): Decoder[VectorMap[K, T]] =
    Decoder.decodeMapLike[K, T, VectorMap]

}
