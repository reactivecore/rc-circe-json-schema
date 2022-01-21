package net.reactivecore.cjs.resolver

/**
  * A path for identifiying elements in JSON
  * RFC 6901 https://datatracker.ietf.org/doc/html/rfc6901
  *
  * @param revElements path elements in reverse order.
  */
case class JsonPointer private (revElements: List[String] = Nil) {

  def enterObject(name: String): JsonPointer = JsonPointer(name :: revElements)

  def enterArray(idx: Int): JsonPointer = JsonPointer(idx.toString :: revElements)

  override def toString: String = {
    revElements
      .reverseMap(JsonPointer.encodeElement)
      .mkString("/", "/", "")
  }

  def startsWith(refUriPath: JsonPointer): Boolean = {
    revElements.endsWith(refUriPath.revElements)
  }
}

object JsonPointer {

  def apply(s: String*): JsonPointer = JsonPointer(s.reverse.toList)

  def fromString(s: String): Option[JsonPointer] = {
    if (s.startsWith("/")) {
      Some(JsonPointer(s.stripPrefix("/").split('/').map(decodeElement).reverse.toList))
    } else {
      None
    }
  }

  private def encodeElement(element: String): String = {
    element
      .replace("~", "~0")
      .replace("/", "~1")
  }

  private def decodeElement(element: String): String = {
    element
      .replace("~1", "/")
      .replace("~0", "~")
  }
}
