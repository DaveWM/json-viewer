package libs

import org.scalajs.dom.Blob

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@JSImport("file-saver", JSImport.Namespace)
@js.native
object fileSaver extends js.Object {
  def saveAs(contents: Blob): Nothing = js.native
  def saveAs(contents: Blob, fileName: String): Nothing = js.native
}
