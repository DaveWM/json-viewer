package jsonviewer

import cats.effect.SyncIO
import colibri.Observable
import io.circe._
import io.circe.parser._
import outwatch.util.Reducer
import jsonviewer.effects._
import jsonviewer.view.savedHtmlView
import libs.snabbdomToHtml
import outwatch.OutWatch

object reducer {

  case class Model(input: String, json: Option[Json], error: Option[String])

  sealed trait Action

  case object Init extends Action

  case class InputChanged(text: String) extends Action

  case object SaveHTML extends Action

  case object NoOp extends Action

  val reducer = Reducer.withOptionalEffects[Observable, Action, Model]((model: Model, action: Action) => {
    action match {
      case Init => (model, None)
      case InputChanged(text) =>
        val newModel = parse(text) match {
          case Left(err) => model.copy(input = text, error = Some(err.message))
          case Right(json) => model.copy(input = text, json = Some(json), error = None)
        }
        (newModel, None)
      case SaveHTML => {
        model.json match {
          case Some(json) => {
            val vdom = savedHtmlView(json)
            val eff = for {
              snabbdom <- OutWatch.toSnabbdom[SyncIO](vdom)
              _ <- saveFile(snabbdomToHtml(snabbdom), "output.html")
            } yield ()
            (model, Some(Observable.fromSync(eff).map(_ => NoOp)))
          }
          case None => (model, None)
        }
      }
      case NoOp => (model, None)
    }
  })
}
