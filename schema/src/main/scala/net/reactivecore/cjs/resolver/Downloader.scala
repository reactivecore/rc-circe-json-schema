package net.reactivecore.cjs.resolver

import cats.MonadError
import cats.implicits._
import io.circe.Json
import io.circe.parser
import net.reactivecore.cjs.{Failure, JsonFailure, ResolveFailure, Result}

import java.io.{BufferedReader, InputStreamReader}
import java.net.URL
import scala.language.higherKinds
import scala.util.Try

trait Downloader[F[_]] {

  /** Load the content of the given URL. */
  def loadJson(url: String): F[Json]
}

object Downloader {
  def empty[F[_]](implicit mo: MonadError[F, Failure]): Downloader[F] = new Downloader[F] {
    override def loadJson(url: String): F[Json] = mo.raiseError(ResolveFailure(s"Empty Downloader"))
  }

  def emptySimple: Downloader[Result] = empty[Result]

  /**
    * Downloads using Java Url Retrievals.
    * Note: use with trusted sources.
    */
  class JavaUrlDownloader extends Downloader[Result] {
    override def loadJson(url: String): Result[Json] = {
      for {
        jUrl <- toUrl(url)
        content <- readUrlToString(jUrl)
        json <- parseJson(url, content)
      } yield json
    }

    private def toUrl(url: String): Result[URL] = {
      Try(new URL(url)).toEither.left.map { error =>
        ResolveFailure(s"Invalid URL ${url}: ${error.getMessage}")
      }
    }

    private def readUrlToString(url: URL): Result[String] = {
      Try {
        val connection = url.openConnection()
        val in = new BufferedReader(new InputStreamReader(connection.getInputStream))
        val buffer = new StringBuilder
        try {
          while ({
            Option(in.readLine()) match {
              case None => false
              case Some(line) =>
                buffer ++= line
                true
            }
          }) ()
        } finally {
          in.close()
        }
        buffer.result()
      }.toEither.left.map { e =>
        ResolveFailure(s"Could not download ${url.toString}", Some(e))
      }
    }

    private def parseJson(url: String, json: String): Result[Json] = {
      parser.parse(json).left.map { parsingFailure =>
        JsonFailure("parsingFailure in ${url}", parsingFailure)
      }
    }
  }
}
