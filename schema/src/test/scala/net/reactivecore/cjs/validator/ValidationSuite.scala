package net.reactivecore.cjs.validator

import io.circe.generic.semiauto
import io.circe.{Codec, Json, parser}
import net.reactivecore.cjs.DownloaderMock
import org.apache.commons.io.FileUtils

import java.io.File
import java.nio.charset.StandardCharsets
import scala.collection.JavaConverters._

object ValidationSuite {

  case class SingleTest(
      description: String,
      data: Json,
      valid: Boolean
  )
  object SingleTest {
    implicit val codec: Codec.AsObject[SingleTest] = semiauto.deriveCodec
  }

  case class SchemaTest(
      description: String,
      schema: Json,
      tests: Vector[SingleTest]
  )

  object SchemaTest {
    implicit val codec: Codec.AsObject[SchemaTest] = semiauto.deriveCodec
  }

  type TestSuite = Vector[SchemaTest]

  def load(name: String): Vector[(String, TestSuite)] = {
    val testDirectory = s"3rdparty/JSON-Schema-Test-Suite/tests/${name}"
    val files = FileUtils.listFiles(new File(DownloaderMock.baseDir, testDirectory), Array("json"), false).asScala
    files
      .map { file =>
        val content = FileUtils.readFileToString(file, StandardCharsets.UTF_8)
        val json = parser.parse(content).getOrElse {
          throw new IllegalStateException(s"Could not parse ${file} JSON")
        }
        val testSuite = json.as[TestSuite].getOrElse {
          throw new IllegalStateException(s"Could not parse ${file} content")
        }
        file.getName -> testSuite
      }
      .toVector
      .sortBy(_._1)
  }
}
