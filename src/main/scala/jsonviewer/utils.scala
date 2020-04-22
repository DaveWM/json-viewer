package jsonviewer

import io.circe.JsonObject

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
}
