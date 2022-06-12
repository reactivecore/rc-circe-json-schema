package net.reactivecore.cjs.restriction

import io.circe.Codec
import net.reactivecore.cjs.Definitions
import net.reactivecore.cjs.util.Codecs
import net.reactivecore.cjs.validator.ValidationProvider

/**
  * Contains all Restrictions.
  */
case class Restriction(
    typeRestriction: DataTypeRestriction = DataTypeRestriction(),
    constRestriction: ConstRestriction = ConstRestriction(),
    enumRestriction: EnumRestriction = EnumRestriction(),
    logicRestrictions: LogicRestrictions = LogicRestrictions(),
    numberRestriction: NumberRestriction = NumberRestriction(),
    objectRestriction: ObjectRestriction = ObjectRestriction(),
    arrayRestriction: ArrayRestriction = ArrayRestriction(),
    stringRestriction: StringRestriction = StringRestriction()
)

object Restriction {
  implicit def codec: Codec.AsObject[Restriction] = Codecs.combineCodecG
  implicit val validationProvider: ValidationProvider[Restriction] = ValidationProvider.combined[Restriction]
}
