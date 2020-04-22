package jsonviewer

import cats.effect.SyncIO
import io.circe.parser._
import jsonviewer.reducer.Model
import io.circe.generic.auto._, io.circe.syntax._

import scala.scalajs.js


object effects {
  val localStorageKey = "json-viewer-state"

  def getStateFromLocalStorage: SyncIO[Option[Model]] =
    SyncIO({
      val storedData: String = js.Dynamic.global.localStorage.getItem(localStorageKey).asInstanceOf[String]
      decode[Model](storedData).toOption
    })

  def saveStateToLocalStorage(model: Model): SyncIO[Unit] =
    SyncIO({
      val serialisedState = model.asJson
      js.Dynamic.global.localStorage.setItem(localStorageKey, serialisedState.toString())
    })
}
