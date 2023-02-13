//> using target.platform "js"

package caseapp.internals

import scala.scalajs.js
import scala.scalajs.js.Dynamic.global

object Argv {
  private lazy val process = global.require("process")
  def get(): Option[Seq[String]] = {
    val argv = process.argv
    if (js.isUndefined(argv) || argv == null) None
    else Some(argv.asInstanceOf[js.Array[String]].drop(2).toSeq)
  }
}
