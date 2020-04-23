package libs

import outwatch.HtmlVNode
import snabbdom.VNodeProxy

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@JSImport("snabbdom-to-html", JSImport.Namespace)
@js.native
object snabbdomToHtml extends js.Object {
  def apply(s: VNodeProxy): String = js.native
}
