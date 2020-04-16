package jsonviewer

import outwatch._
import outwatch.dsl._
import colibri.{Observable, Observer}
import outwatch.util.{Reducer, Store}
import cats.effect.IO

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import scala.scalajs.js.Thenable.Implicits._
import scala.scalajs.js
import scala.scalajs.js.Promise
import play.api.libs.json.{JsNull, _}

import scala.util.Try

case class Model(counter: Int, json: Option[JsValue])

sealed trait Action

case object Init extends Action

case object ImportFromClipboard extends Action

case class ImportComplete(text: String) extends Action

object Jsonviewer {

  def main(args: Array[String]): Unit = {
    val reducer = Reducer.withOptionalEffects[Observable, Action, Model]((model: Model, action: Action) => {
      println(model, action)
      action match {
        case Init => (model, None)
        case ImportFromClipboard =>
          val clipboardContents: Promise[String] = js.Dynamic.global.navigator.clipboard.readText().asInstanceOf[Promise[String]]
          (model, Some(Observable.fromFuture(clipboardContents.toFuture).map(ImportComplete)))
        case ImportComplete(text) =>
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

    def renderJson(json: JsValue): HtmlVNode = json match {
      case JsNull => span("Null!")
      case JsNumber(n) => span(n.toString())
      case JsBoolean(b) => span(if (b) "True" else "False")
      case JsString(s) => span(s)
      case JsObject(kvs) => dl(cls := "uk-description-list json-object-display").apply(
        kvs.flatMap {
          case (k, v) => List(
            dt(renderJson(JsString(k))),
            dd(div(renderJson(v)))
          )
        }.toList
      )
      case JsArray(xs) =>
        xs.toList match {
          case x :: Nil => renderJson(x)
          case List() => span("<< Empty List >>")
          case _ if xs.length < 5 && xs.forall(_.isInstanceOf[JsObject]) =>
            div(
              ul(attr("data-uk-tab") := "").apply(
                xs.zipWithIndex.map {
                  case (value, idx) => li(a(href := "#", s"Item $idx"))
                }.toList
              ),
              ul(cls := "uk-switcher uk-margin").apply(
                xs.map(x => li(renderJson(x))).toList
              )
            )
          case _ if xs.length < 5 && xs.forall(x => x.isInstanceOf[JsNumber] || x.isInstanceOf[JsString] || x.isInstanceOf[JsBoolean] || x == JsNull) =>
            div(cls := "horiz-list").apply(
              intersperse(xs.map(renderJson).toList, span(", "))
            )
          case x :: rest => {
            ul(cls := "uk-list uk-list-bullet").apply(
              xs.map(x => li(renderJson(x))).toList
            )
          }
        }
    }

    def view(state: Model, dispatch: Observer[Action]) =
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
            button(
              cls := "uk-button uk-button-primary", onClick.use(ImportFromClipboard) --> dispatch,
              "Import from Clipboard"
            ),
            div(
              cls := "uk-card uk-card-default uk-card-body json-display",
              state.json.map(renderJson).getOrElse(p(cls := "uk-lead", "Please import some JSON to get started"))
            )
          )
        )
      )

    val app = for {
      store <- Store.create[IO](Init, new Model(0, None), reducer)
      viewStream = store.map(am => view(am._2, store))
      _ = viewStream.foreach(_ => js.Dynamic.global.window.mdc.autoInit())
      _ <- OutWatch.renderInto[IO]("#app", div(viewStream))
    } yield ()

    app.unsafeRunSync()
  }
}
