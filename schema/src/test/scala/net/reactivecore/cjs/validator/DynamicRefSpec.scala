package net.reactivecore.cjs.validator

/** Duplicates DynmicRef.json test for better debugging */
class DynamicRefSpec extends ValidatorTestBase {
  val stringArray = """["foo", "bar"]"""
  val mixedArray = """["foo", 42]"""

  val schema =
    """
      |{
      |            "$id": "https://test.json-schema.org/dynamic-resolution-with-intermediate-scopes/root",
      |            "$ref": "intermediate-scope",
      |            "$defs": {
      |                "foo": {
      |                    "$dynamicAnchor": "items",
      |                    "type": "string"
      |                },
      |                "intermediate-scope": {
      |                    "$id": "intermediate-scope",
      |                    "$ref": "list"
      |                },
      |                "list": {
      |                    "$id": "list",
      |                    "type": "array",
      |                    "items": { "$dynamicRef": "#items" },
      |                    "$defs": {
      |                      "items": {
      |                          "$comment": "This is only needed to satisfy the bookending requirement",
      |                          "$dynamicAnchor": "items"
      |                      }
      |                    }
      |                }
      |            }
      |        }
      |""".stripMargin

  it should "figure out when it's ok" in {
    testSchemaValidation(schema, stringArray, true)
  }

  it should s"figure out when it is not ok" in {
    testSchemaValidation(schema, mixedArray, false)
  }

  val schema2 =
    """{
      |            "$id": "https://test.json-schema.org/dynamic-resolution-without-bookend/root",
      |            "$ref": "list",
      |            "$defs": {
      |                "foo": {
      |                    "$dynamicAnchor": "items",
      |                    "type": "string"
      |                },
      |                "list": {
      |                    "$id": "list",
      |                    "type": "array",
      |                    "items": { "$dynamicRef": "#items" },
      |                    "$defs": {
      |                        "items": {
      |                            "$comment": "This is only needed to give the reference somewhere to resolve to when it behaves like $ref",
      |                            "$anchor": "items"
      |                        }
      |                    }
      |                }
      |            }
      |        }            
      |""".stripMargin

  "schema2" should "accept a string of any" in {
    testSchemaValidation(schema2, mixedArray, true)
  }

  val schema3 =
    """
      |{
      |            "$id": "https://test.json-schema.org/dynamic-ref-with-multiple-paths/main",
      |            "$defs": {
      |                "inner": {
      |                    "$id": "inner",
      |                    "$dynamicAnchor": "foo",
      |                    "title": "inner",
      |                    "additionalProperties": {
      |                        "$dynamicRef": "#foo"
      |                    }
      |                }
      |            },
      |            "if": {
      |                "propertyNames": {
      |                    "pattern": "^[a-m]"
      |                }
      |            },
      |            "then": {
      |                "title": "any type of node",
      |                "$id": "anyLeafNode",
      |                "$dynamicAnchor": "foo",
      |                "$ref": "main#/$defs/inner"
      |            },
      |            "else": {
      |                "title": "integer node",
      |                "$id": "integerNode",
      |                "$dynamicAnchor": "foo",
      |                "type": [ "object", "integer" ],
      |                "$ref": "main#/$defs/inner"
      |            }
      |        }""".stripMargin

  "schema3" should "deny a float" in {
    testSchemaValidation(schema3, """{ "november": 1.1 }""", false)
  }
}
