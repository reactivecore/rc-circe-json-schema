package net.reactivecore.cjs

import net.reactivecore.cjs.resolver.JsonPointer
import net.reactivecore.cjs.validator.{SchemaValidator, Validator}

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable

/** Validator for a single document within [[DocumentValidator]] */
case class SingleDocumentValidator(
    schema: Schema,
    validator: SchemaValidator
) {
  val fragments: Map[String, Validator] = collectFragments()

  val dynamicFragments: Map[String, Validator] = {
    val collector = mutable.Map[String, Validator]()
    validator.wideForeach {
      case v: SchemaValidator =>
        v.dynamicFragment.filterNot(collector.contains).foreach { fragment =>
          collector += (fragment -> v)
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

  private def collectFragments(): Map[String, Validator] = {
    val collector = Seq.newBuilder[(String, Validator)]

    /* Traverse sub validators as long as they do not have their own id. */
    def handle(validator: Validator, first: Boolean): Unit = {
      validator match {
        case s: SchemaValidator if first || s.idOverride.isEmpty =>
          s.fragment.foreach { fragment =>
            collector += (fragment -> s)
          }
          s.children.foreach(handle(_, first = false))
        case _ => validator.children.foreach(handle(_, first = false))
      }
    }
    handle(validator, first = true)
    collector.result().toMap
  }
}
