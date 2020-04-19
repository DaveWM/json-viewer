package jsonviewer

import cats.effect.IO
import colibri.{Observable, Observer}
import outwatch._
import outwatch.dsl._
import outwatch.util.{Reducer, Store}
import play.api.libs.json.{JsNull, _}

import scala.collection.immutable._
import scala.concurrent.ExecutionContext.Implicits._
import scala.scalajs.js
import scala.scalajs.js.Promise
import scala.util.{Failure, Success, Try}
import libs.humanizeString

case class Model(counter: Int, json: Option[JsValue])

sealed trait Action

case object Init extends Action

case class InputChanged(text: String) extends Action

object Jsonviewer {

  def main(args: Array[String]): Unit = {
    val reducer = Reducer.withOptionalEffects[Observable, Action, Model]((model: Model, action: Action) => {
      action match {
        case Init => (model, None)
        case InputChanged(text) =>
          val json = Try {
            Json.parse(text)
          }.toEither
          val newModel = json match {
            case Left(_) => model
            case Right(json) => model.copy(json = Some(json))
          }
          (newModel, None)
      }
    })

    def intersperse[T](xs: List[T], separator: T): List[T] = {
      xs match {
        case x :: Nil => List(x)
        case x :: rest => x :: separator :: intersperse(rest, separator)
        case _ => List.empty
      }
    }

    def getAllProps(xs: List[JsObject]): Set[String] = {
      xs.flatMap {
      case JsObject(m) => m.keySet
      case _ => Set.empty
    }.foldRight(Set.empty[String])((k, ks: Set[String]) => ks + k)
    }

    def canRenderAsTable(xs: List[JsValue]): Boolean = {
      val allProps = getAllProps(xs.collect {
        case x: JsObject => x
      })

      val sharedProps = allProps.filter(p => {
        xs.forall {
          case JsObject(m) => m.keySet.contains(p)
          case _ => false
        }
      })

      xs.forall(v =>
        v.isInstanceOf[JsObject] &&
        (sharedProps.size >= allProps.size - 5) &&
        xs.forall {
          case JsObject(m) => m.values.forall(v => !v.isInstanceOf[JsObject] && !v.isInstanceOf[JsArray])
        }
      )
    }

    def renderJson(json: JsValue, level: Int): HtmlVNode = json match {
      case JsNull => span("Null!")
      case JsNumber(n) => span(n.toString())
      case JsBoolean(b) => {
        val icon = if (b) "check" else "close"
        val colour = if (b) "limegreen" else "red"
        span(attr("uk-icon") := icon, style("color") := colour)
      }
      case JsString(s) => span(s)
      case JsObject(kvs) => dl(cls := s"uk-description-list json-object-display level-$level").apply(
        kvs.flatMap {
          case (k, v) => List(
            dt(humanizeString(k)),
            dd(div(renderJson(v, level + 1)))
          )
        }.toList
      )
      case JsArray(xs) =>
        xs.toList match {
          case x :: Nil => renderJson(x, level + 1)
          case List() => span(cls := "uk-label", "Empty")
          case xs if xs.length < 5 && xs.forall(_.isInstanceOf[JsObject]) =>
            div(
              ul(attr("data-uk-tab") := "animation: uk-animation-fade").apply(
                xs.zipWithIndex.map {
                  case (value, idx) => li(a(href := "#", s"Item $idx"))
                }
              ),
              ul(cls := "uk-switcher uk-margin").apply(
                xs.map(x => li(renderJson(x, level + 1)))
              )
            )
          case xs if xs.length < 5 && xs.forall(x => x.isInstanceOf[JsNumber] || x.isInstanceOf[JsString] || x.isInstanceOf[JsBoolean] || x == JsNull) =>
            div(cls := "horiz-list").apply(
              intersperse(xs.map(renderJson(_, level + 1)), span(", "))
            )
          case xs if canRenderAsTable(xs) => {
            val childObjects = xs.collect { case x: JsObject => x}
            val props = getAllProps(childObjects)
            table(
              cls := "uk-table uk-table-divider",
              tr.apply(props.map(s => th(humanizeString(s))).toList),
              childObjects.map {
                case JsObject(m) =>
                  tr.apply(
                    props.map(p => td(m.get(p).map(renderJson(_, level + 1)).getOrElse(""))).toList
                  )
              }
            )
          }

          case x :: rest =>
            ul(cls := "uk-list uk-list-bullet").apply(
              xs.map(x => li(renderJson(x, level + 1))).toList
            )
        }
    }

    def view(state: Model, dispatch: Observer[Action]) = {
      val tryView: Try[HtmlVNode] = Try {
        div(
          div(
            cls := "uk-navbar-container",
            id := "header-bar",
            div(
              cls := "uk-navbar-left",
              div(
                cls := "uk-navbar-item uk-logo logo",
                "JSON Viewer"
              )
            )
          ),
          div(
            cls := "uk-section",
            div(
              cls := "uk-container",
              textArea(
                cls := "uk-textarea",
                placeholder := "Enter JSON here",
                rows := 5,
                onInput.value.map(InputChanged) --> dispatch
              ),
              state.json.map(json => {
                div(
                  cls := "uk-card uk-card-default uk-card-body json-display",
                  renderJson(json, 0)
                )
              })
            )
          )
        )
      }

      tryView match {
        case Success(v) => v
        case Failure(f) => div("something broke: ", f.toString)
      }
    }

    val app = for {
      store <- Store.create[IO](Init, new Model(0, None), reducer)
      viewStream = store.map(am => view(am._2, store))
      _ = viewStream.foreach(_ => js.Dynamic.global.window.mdc.autoInit())
      _ <- OutWatch.renderInto[IO]("#app", div(viewStream))
    } yield ()

    app.unsafeRunSync()
  }
}
