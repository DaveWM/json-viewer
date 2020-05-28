package jsonviewer

import cats.effect.SyncIO
import io.circe.parser._
import jsonviewer.reducer.Model
import io.circe.generic.auto._
import io.circe.syntax._
import libs.fileSaver
import org.scalajs.dom.Blob
import org.scalajs.dom.raw.BlobPropertyBag

import scala.scalajs.js


object effects {
  val localStorageKey = "json-viewer-state-2"

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

  def saveFile(contents: String, fileName: String): SyncIO[Unit] =
    SyncIO({
      val blob = new Blob(js.Array(contents), BlobPropertyBag("text/plain; charset=utf-8"))
      fileSaver.saveAs(blob, fileName)
    })
}
