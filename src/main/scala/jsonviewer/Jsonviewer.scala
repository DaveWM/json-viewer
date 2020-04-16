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
import io.circe._, io.circe.parser._

case class Model(counter: Int, json: Option[Json])

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
          val json = parse(text)
          val newModel = json match {
            case Left(_) => model
            case Right(json) => model.copy(json = Some(json))
          }
          (newModel, None)
      }
    })

    def view(state: Model, dispatch: Observer[Action]) =
      div(
        h1("JSON Viewer"),
        button(onClick.use(ImportFromClipboard) --> dispatch , "Import from Clipboard"),
        div(state.json.toString())
      )

    val app = for {
      store <- Store.create[IO](Init, new Model(0, None), reducer)
      viewStream = store.map(am => view(am._2, store))
      _ <- OutWatch.renderInto[IO]("#app", div(viewStream))
    } yield ()

    app.unsafeRunSync()
  }
}
