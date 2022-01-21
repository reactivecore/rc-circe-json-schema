package net.reactivecore.cjs.restriction

import io.circe.Codec
import io.circe.generic.semiauto
import net.reactivecore.cjs.resolver.{JsonPointer, RefUri}
import net.reactivecore.cjs.util.Codecs
import net.reactivecore.cjs.validator._
import net.reactivecore.cjs.Schema

/** Further restrictions of a type. */
case class LogicRestrictions(
    oneOf: Option[Vector[Schema]] = None,
    anyOf: Option[Vector[Schema]] = None,
    not: Option[Schema] = None,
    allOf: Option[Vector[Schema]] = None,
    `if`: Option[Schema] = None,
    `then`: Option[Schema] = None,
    `else`: Option[Schema] = None
)

object LogicRestrictions {
  implicit lazy val codec: Codec.AsObject[LogicRestrictions] = Codecs.withoutNulls(semiauto.deriveCodec)

  implicit lazy val validationProvider: ValidationProvider[LogicRestrictions] = ValidationProvider.withUri {
    (parentId, path, restrictions) =>
      allOf(
        logicChain(parentId, path.enterObject("oneOf"), restrictions.oneOf, x => OneOfValidator(x.toVector)),
        logicChain(parentId, path.enterObject("anyOf"), restrictions.anyOf, x => AnyOfValidator(x.toVector)),
        logicChain(parentId, path.enterObject("allOf"), restrictions.allOf, x => AllOfValidator(x.toVector)),
        restrictions.not
          .map { schema =>
            NotValidator(schema.validator(parentId, path))
          }
          .getOrElse(Validator.success),
        ifThenElse(parentId, path, restrictions)
      )
  }

  /** Note: Logic operators are combined using all of and not run sequentially. */
  private def allOf(validators: Validator*): Validator = {
    val withoutTrue = validators.filterNot(_ == Validator.success)
    withoutTrue.size match {
      case 0 => Validator.success
      case 1 => withoutTrue.head
      case _ => AllOfValidator(withoutTrue.toVector)
    }
  }

  private def ifThenElse(parentId: RefUri, path: JsonPointer, logicRestrictions: LogicRestrictions): Validator = {
    val got = for {
      i <- logicRestrictions.`if`
    } yield {
      val ic = i.validator(parentId, path)
      val t = logicRestrictions.`then`.map(_.validator(parentId, path))
      val e = logicRestrictions.`else`.map(_.validator(parentId, path))
      IfThenElseValidator(ic, t, e)
    }
    got.getOrElse(Validator.success)
  }

  private def logicChain(
      parentId: RefUri,
      path: JsonPointer,
      input: Option[Vector[Schema]],
      combiner: Seq[Validator] => Validator
  ): Validator = {
    input match {
      case None => Validator.success
      case Some(schemas) =>
        val validators = schemas.zipWithIndex.map { case (schema, idx) =>
          schema.validator(parentId, path.enterArray(idx))
        }
        combiner(validators)
    }
  }
}
