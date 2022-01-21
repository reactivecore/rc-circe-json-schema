package net.reactivecore.cjs

import net.reactivecore.cjs.resolver.JsonPointer
import net.reactivecore.cjs.validator.{SchemaValidator, Validator}

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable

/** Validator for a single document within [[DocumentValidator]] */
case class SingleDocumentValidator(
    schema: Schema,
    validator: Validator
) {
  val fragments: Map[String, Validator] = {
    collectFragments {
      case v: SchemaValidator => v.fragment
      case _                  => None
    }
  }

  val dynamicFragments: Map[String, Validator] = {
    val collector = mutable.Map[String, Validator]()
    validator.wideForeach {
      case v: SchemaValidator =>
        v.dynamicFragment match {
          case Some(given) if !collector.contains(given) =>
            collector += (given -> v)
          case Some(given) => // nothing
          case None        => // nothing
        }
      case _ => None
    }
    collector.toMap
  }

  def findPath(path: JsonPointer): Option[Validator] = {
    pathCache.computeIfAbsent(path, path => findPathRec(validator, path))
  }

  private val pathCache = new ConcurrentHashMap[JsonPointer, Option[Validator]]()

  private def findPathRec(validator: Validator, path: JsonPointer): Option[Validator] = {
    def lookChildren(validator: Validator): Option[Validator] = {
      val it = validator.children.iterator
      while (it.hasNext) {
        findPathRec(it.next(), path) match {
          case Some(found) => return Some(found)
          case None        => // continue
        }
      }
      None
    }

    validator match {
      case s: SchemaValidator if path.startsWith(s.path) =>
        if (s.path == path) {
          Some(s)
        } else {
          lookChildren(s)
        }
      case s: SchemaValidator => None
      case other: Validator =>
        lookChildren(other)
    }
  }

  private def collectFragments(f: Validator => Option[String]): Map[String, Validator] = {
    val collector = Seq.newBuilder[(String, Validator)]
    validator.deepForeach { validator =>
      f(validator) match {
        case Some(given) => collector += given -> validator
        case None        => // nothing
      }
    }
    collector.result().toMap
  }
}
