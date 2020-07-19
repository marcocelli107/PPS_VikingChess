package model.prolog
import org.apache.commons.io.IOUtils
import java.io.StringWriter

object GameRules {

  val THEORY: String = "/prolog/gameRules.pl"

  def theory(): String = {
    val writer = new StringWriter
    IOUtils.copy(getClass.getResourceAsStream(THEORY), writer, "utf8")
    writer.toString
  }

}
