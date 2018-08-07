package stanzas

import com.github.rgafiyatullin.xml.dom.Node
import com.github.rgafiyatullin.xmpp_protocol.stanzas.Stanza
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success

trait StanzasTest extends FlatSpec with Matchers {
  def renderTest[S <: Stanza](stanza: S, rendered: Node): Unit = {
    stanza.toXml should be (rendered)
    ()
  }

  def parseTest[S <: Stanza](decoder: Stanza.Decoder[S])(xml: Node, stanza: S): Unit = {
    decoder(xml).get should be (stanza)
    ()
  }
}
