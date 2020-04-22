package jsonviewer

import io.circe.JsonObject
import outwatch.{HtmlVNode, VDomModifier}
import outwatch.dsl._

object utils {
  def intersperse[T](xs: List[T], separator: T): List[T] = {
    xs match {
      case x :: Nil => List(x)
      case x :: rest => x :: separator :: intersperse(rest, separator)
      case _ => List.empty
    }
  }

  def getAllProps(xs: List[JsonObject]): Set[String] = {
    xs.flatMap(_.keys).toSet
  }

  def linkifyText(text: String): HtmlVNode = {
    val linkRegex = "https?://[^\\s]+".r
    val linkRegexLookahead = "(?=(https?://[^\\s]+))".r
    span.apply(
      linkRegexLookahead.split(text)
        .flatMap[VDomModifier]((segment: String) => {
            linkRegex.findFirstMatchIn(segment) match {
              case Some(m) => {
                val url = m.matched
                List(a(href := url, target := "_blank", url), segment.replace(url, ""))
              }
              case None => List(segment)
            }
        })
    )
  }
}
