package net.reactivecore.cjs.validator

import io.circe.syntax._
import net.reactivecore.cjs.{DownloaderMock, Schema, TestBase}
import cats.implicits._

class ValidationSuiteSpec extends TestBase {

  behavior of ("Parsing")

  val unknownKeywords = Seq(
    "unknownKeyword.json" // Unknown properties are not serialized again
  )

  val unsupported = Seq(
    "vocabulary.json"
  )

  val tests = ValidationSuite.suite2020_12.filterNot { case (name, _) =>
    unsupported.contains(name)
  }

  tests.foreach { case (name, suite) =>
    it should s"Parse and serialize ${name}" in {
      suite.foreach { schemaTest =>
        val schema = schemaTest.schema.as[Schema] match {
          case Left(error) =>
            fail(s"${error} in ${schemaTest.schema}")
          case Right(ok) => ok
        }
        schema.asJson.as[Schema] shouldBe Right(schema)
        if (!(unknownKeywords.exists(b => name.contains(b)))) {
          ensureJsonEqual(schema.asJson, schemaTest.schema)
        }
      }
    }

    it should s"resolve all in ${name}" in {
      suite.foreach { schemaTest =>
        val schema = schemaTest.schema.as[Schema].forceRight
        withClue(s"in schema ${schemaTest.description}") {
          schema.resolve(new DownloaderMock).forceRight
          // resolved.asJson.as[Schema] shouldBe Right(resolved)
        }
      }
    }

    it should s"validate correctly ${name}" in {
      suite.zipWithIndex.foreach { case (schemaTest, schemaIdx) =>
        val schema = schemaTest.schema.as[Schema] match {
          case Left(error) =>
            fail(s"${error} in ${schemaTest.schema}")
          case Right(ok) => ok
        }
        val validator = schema.resolve(new DownloaderMock).forceRight
        schemaTest.tests.zipWithIndex.foreach { case (singleTest, testIdx) =>
          val violations = validator.validate(singleTest.data)
          val isOk = violations.isSuccess == singleTest.valid
          if (isOk) {
            // println(s"Passing ${schemaTest.description}/${schema.description.description}/${singleTest.description}")
          } else {

            val clue =
              s"""
                 |Validating ${singleTest.data} using schema
                 |
                 |${schema.asJson}
                 |
                 |Test:       ${singleTest.description} ${testIdx + 1}/${schemaTest.tests.size}
                 |Schema:      ${schema.description.description} (${schemaIdx + 1} / ${suite.size})
                 |SchemaTest:  ${schemaTest.description}
                 |
                 |
                 |""".stripMargin

            withClue(clue) {
              validator.validate(singleTest.data)
              if (singleTest.valid) {
                violations shouldBe 'success
              } else {
                violations shouldBe 'failure
              }
            }
          }
        }
      }
    }
  }
}
