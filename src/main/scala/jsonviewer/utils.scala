package jsonviewer

import io.circe.JsonObject
import libs.date
import outwatch.{HtmlVNode, VDomModifier}
import outwatch.dsl._

import scala.scalajs.js
import scala.scalajs.js.Date
import scala.util.Try

object utils {
  def intersperse[T](xs: List[T], separator: T): List[T] = {
    xs match {
      case x :: Nil => List(x)
      case x :: rest => x :: separator :: intersperse(rest, separator)
      case _ => List.empty
    }
  }

  def getAllProps(xs: List[JsonObject]): Set[String] = {
    println(xs.flatMap(_.keys).toSet)
    xs.flatMap(_.keys).toSet
  }

  def linkifyText(text: String): List[VDomModifier] = {
    val linkRegex = "https?://[^\\s]+".r
    val linkRegexLookahead = s"(?=($linkRegex))".r
    linkRegexLookahead.split(text)
      .flatMap[VDomModifier]((segment: String) => {
        linkRegex.findFirstMatchIn(segment) match {
          case Some(m) =>
            val url = m.matched
            List(a(href := url, target := "_blank", url), segment.replace(url, ""))
          case None => List(segment)
        }
      })
      .toList
  }

  def emailifyText(text: String): List[VDomModifier] = {
    val emailRegex = "[a-zA-Z0-9.!#$%&’*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*".r
    val emailRegexLookahead = s"\\s(?=($emailRegex))".r
    emailRegexLookahead.split(text)
      .flatMap[VDomModifier]((segment: String) => {
        emailRegex.findFirstMatchIn(segment) match {
          case Some(m) =>
            val email = m.matched
            List(" ", a(href := s"mailto:$email", email), segment.replace(email, ""))
          case None => {
            List(segment)
          }
        }
      })
      .toList
  }

  def tryParseDate(s: String): Option[date] = {
    if (s.length < 4)
      return None

    val maybeInt = Try {
      Integer.parseInt(s);
    }.toOption

    maybeInt match {
      case None => Option(js.Dynamic.global.Date.parse(s).asInstanceOf[date])
      case Some(_) => None
    }
  }

  def formatDate(d: date): String = {
    val isDate = d.getHours() == 0 && d.getMinutes() == 0 && d.getSeconds() == 0 && d.getMilliseconds() == 0
    if (isDate)
      d.toString("dd MMM yyyy")
    else
      d.toString("dd MMM yyyy, HH:mm")
  }
}
