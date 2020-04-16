package jsonviewer

import org.scalatest._
import org.scalajs.dom._
import outwatch._
import outwatch.dsl._

import cats.effect.IO

class JsonviewerSpec extends JSDomSpec {

  "You" should "probably add some tests" in {

    val message = "Hello World!"
    OutWatch.renderInto[IO]("#app", h1(message)).unsafeRunSync()

    document.body.innerHTML.contains(message) shouldBe true
  }
}
