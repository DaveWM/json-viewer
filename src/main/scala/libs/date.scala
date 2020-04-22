package libs

import scala.scalajs.js
import scala.scalajs.js.Date

@js.native
trait date extends Date {
  def toString(s: String): String = ???
}

object date {
  implicit def date(d: Date): date =
    d.asInstanceOf[date]
}
