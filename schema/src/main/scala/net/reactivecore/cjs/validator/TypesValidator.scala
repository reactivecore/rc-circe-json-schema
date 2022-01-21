package net.reactivecore.cjs.validator

import io.circe.Json
import net.reactivecore.cjs.DataTypeName

case class TypesValidator(types: Vector[DataTypeName]) extends SimpleContextFreeValidator(s"DataTypes ${types}") {

  override def isValid(json: Json): Boolean = {
    types.exists {
      case DataTypeName.DtString  => json.isString
      case DataTypeName.DtArray   => json.isArray
      case DataTypeName.DtObject  => json.isObject
      case DataTypeName.DtBoolean => json.isBoolean
      case DataTypeName.DtNumber  => json.isNumber
      case DataTypeName.DtNull    => json.isNull
      case DataTypeName.DtInteger => {
        json.isNumber && json.as[Long].isRight
      }
    }
  }
}
