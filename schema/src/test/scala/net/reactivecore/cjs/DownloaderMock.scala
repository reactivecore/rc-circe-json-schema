package net.reactivecore.cjs

import io.circe.Json
import net.reactivecore.cjs.resolver.{Downloader, ResolveError, SimpleResolveResult}
import org.apache.commons.io.FileUtils

import java.io.{File, FileNotFoundException}
import java.nio.charset.StandardCharsets
import scala.util.Try

/** Fakes a downloader with test files */
class DownloaderMock extends Downloader[SimpleResolveResult] {

  val directories = Seq(
    "https://json-schema.org/draft/2020-12" -> "3rdparty/schema/2020-12",
    "http://localhost:1234" -> "3rdparty/JSON-Schema-Test-Suite/remotes"
  )

  override def loadJson(url: String): SimpleResolveResult[Json] = {
    for {
      location <- directories
        .collectFirst {
          case (prefix, path) if url.startsWith(prefix) => path + url.stripPrefix(prefix)
        }
        .toRight(ResolveError(s"File ${url} not found"))
      content <- Try(
        FileUtils.readFileToString(new File(DownloaderMock.baseDir, location), StandardCharsets.UTF_8)
      ).toEither.left
        .map { error =>
          ResolveError(error.getMessage)
        }
      json <- io.circe.parser
        .parse(content)
        .left
        .map(err => ResolveError(s"Invalid JSON on ${url}: ${err.getMessage()}"))
    } yield json
  }
}

object DownloaderMock {

  /**
    * Figures out the base directory of the repository.
    * (IntelliJ and SBT are using different base directories)
    */
  lazy val baseDir = {
    val candidates = Seq(
      new File("./"),
      new File("../")
    )
    candidates.find { dir => new File(dir, "3rdparty").isDirectory }.getOrElse {
      throw new FileNotFoundException(s"Could not find base directory")
    }
  }
}
