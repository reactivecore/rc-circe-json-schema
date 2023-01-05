package net.reactivecore.cjs.resolver

import io.circe.{Decoder, Encoder}

import java.net.{URI, URISyntaxException}

/** Wraps Javas' URI but feels more scala like */
case class RefUri(
    scheme: Option[String] = None,
    userInfo: Option[String] = None,
    host: Option[String] = None,
    port: Option[Int] = None,
    path: Option[String] = None,
    query: Option[String] = None,
    fragment: Option[String] = None,
    schemeSpecificPart: Option[String] = None
) {

  /** Converts to Java URI */
  def toUri: URI = {
    schemeSpecificPart match {
      case Some(value) => new URI(scheme.orNull, value, fragment.orNull)
      case None =>
        new URI(
          scheme.orNull,
          userInfo.orNull,
          host.orNull,
          port match {
            case None    => -1
            case Some(n) => n
          },
          path.orNull,
          query.orNull,
          fragment.orNull
        )
    }
  }

  /** Resolve an uri. */
  def resolve(other: RefUri): RefUri = {
    /* Workaround for resolving pure fragments on URN-Uris, otherwise it would just return other */
    if (other.fragment.exists(_.nonEmpty) && other.copy(fragment = None).isEmpty) {
      return copy(fragment = other.fragment)
    }
    RefUri.fromUri(
      toUri.resolve(other.toUri)
    )
  }

  /** Returns true if this RefUri is empty. */
  def isEmpty: Boolean = {
    this == RefUri.empty
  }

  /** Resolve if other is set (convenience function) */
  def maybeResolve(other: Option[RefUri]): RefUri = {
    other.map { resolve }.getOrElse(this)
  }

  /** Append path like fragments. If there is currently not a path like one, it will be replaced. */
  def appendPathFragment(path: String): RefUri = {
    val parts = path.split('/').toSeq
    parts.foldLeft(this) { case (current, next) =>
      current.appendSinglePathFragment(next)
    }
  }

  private def appendSinglePathFragment(fragment: String): RefUri = {
    this.fragment match {
      case Some(existing) if existing.startsWith("/") =>
        copy(fragment = Some(existing + "/" + fragment))
      case otherwise =>
        copy(fragment = Some("/" + fragment))
    }
  }

  /** Converts to canonical string */
  override def toString: String = {
    toUri.toString
  }
}

object RefUri {

  def empty: RefUri = RefUri()

  def fromString(s: String): Either[String, RefUri] = {
    try {
      Right(forceString(s))
    } catch {
      case s: URISyntaxException => Left(Option(s.getMessage).getOrElse(s"Unknown error"))
    }
  }

  def forceString(s: String): RefUri = {
    fromUri(new URI(s))
  }

  def fromUri(uri: URI): RefUri = {
    RefUri(
      scheme = Option(uri.getScheme),
      userInfo = Option(uri.getUserInfo),
      host = Option(uri.getHost),
      port = Some(uri.getPort).filter(_ != -1),
      path = Option(uri.getPath).filter(_.nonEmpty),
      query = Option(uri.getQuery),
      fragment = Option(uri.getFragment),
      schemeSpecificPart = Option(uri.getSchemeSpecificPart).filter(_.nonEmpty)
    )
  }

  implicit val decoder: Decoder[RefUri] = Decoder.decodeString.emap {
    RefUri.fromString
  }

  implicit val encoder: Encoder[RefUri] = Encoder.encodeString.contramap(_.toString)
}
