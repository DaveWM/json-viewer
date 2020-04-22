package jsonviewer

import cats.effect.SyncIO
import jsonviewer.effects._
import jsonviewer.reducer._
import jsonviewer.view._
import outwatch._
import outwatch.dsl._
import outwatch.util.Store
import scala.scalajs.js

object Jsonviewer {

  def main(args: Array[String]): Unit = {
    val app = for {
      model <- getStateFromLocalStorage
      store <- Store.create[SyncIO](Init, model.getOrElse(new Model("", None, None)), reducer)
      viewStream = store.map(am => view(am._2, store))
      _ = viewStream.foreach(_ => js.Dynamic.global.window.mdc.autoInit())
      _ = store.mapSync {
        case (action, model) => saveStateToLocalStorage(model)
      }.foreach(_ => ())
      _ <- OutWatch.renderInto[SyncIO]("#app", div(viewStream))
    } yield ()

    app.unsafeRunSync()
  }

}
