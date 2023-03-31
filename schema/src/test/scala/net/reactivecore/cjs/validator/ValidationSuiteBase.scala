package net.reactivecore.cjs.validator

import io.circe.Json
import net.reactivecore.cjs.{DownloaderMock, Loader, Schema, TestBase, Vocabulary}
import io.circe.syntax._

abstract class ValidationSuiteBase(name: String, defaultVocabulary: Vocabulary) extends TestBase {

  behavior of ("Parsing")

  val unsupported = Seq(
  )

  val tests = ValidationSuite.load(name).filterNot { case (name, _) =>
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
      }
    }

    it should s"resolve all in ${name}" in {
      suite.foreach { schemaTest =>
        withClue(s"in schema ${schemaTest.description}") {
          new Loader(new DownloaderMock, defaultVocabulary).fromJson(schemaTest.schema).forceRight
        }
      }
    }

    it should s"validate correctly ${name}" in {
      suite.zipWithIndex.foreach { case (schemaTest, schemaIdx) =>
        val validator = new Loader(new DownloaderMock, defaultVocabulary).fromJson(schemaTest.schema).forceRight
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
                 |${schemaTest.schema}
                 |
                 |Test:       ${singleTest.description} ${testIdx + 1}/${schemaTest.tests.size}
                 |Used Schema:
                 |
                 |${validator.mainSchema.asJson}
                 |
                 |Schema:      ${validator.mainSchema.description.description} (${schemaIdx + 1} / ${suite.size})
                 |SchemaTest:  ${schemaTest.description}
                 |
                 |
                 |""".stripMargin

            withClue(clue) {
              validator.validate(singleTest.data)
              if (singleTest.valid) {
                violations.isSuccess shouldBe true
              } else {
                violations.isFailure shouldBe true
              }
            }
          }
        }
      }
    }
  }
}
