package jsonviewer

import colibri.Observable
import io.circe._
import io.circe.parser._
import outwatch.util.Reducer

object reducer {
  case class Model(input: String, json: Option[Json], error: Option[String])

  sealed trait Action

  case object Init extends Action

  case class InputChanged(text: String) extends Action

  val reducer = Reducer.withOptionalEffects[Observable, Action, Model]((model: Model, action: Action) => {
    action match {
      case Init => (model, None)
      case InputChanged(text) =>
        val newModel = parse(text) match {
          case Left(err) => model.copy(input = text, error = Some(err.message))
          case Right(json) => model.copy(input = text, json = Some(json), error = None)
        }
        (newModel, None)
    }
  })
}
