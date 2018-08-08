package stanzas

import com.github.rgafiyatullin.xml.dom.Node
import com.github.rgafiyatullin.xmpp_protocol.XmppConstants
import com.github.rgafiyatullin.xmpp_protocol.stanzas.Stanza
import com.github.rgafiyatullin.xmpp_protocol.stanzas.sasl.Sasl

final class SaslTest extends StanzasTest {
  import XmppConstants.names.urn.ietf.params.xmlNs.{xmppSasl => consts}

  val mechanism: String = "mechanism"
  val plain: String = "PLAIN"
  val data: String = "aGVsbG8gd29ybGQ="
  val emptyCData: Node = Node("")
  lazy val decoder: Stanza.Decoder[Sasl] = Sasl.decode

  val tests: Seq[(Sasl, Node)] = Seq(
    Sasl.Auth() ->
      Node(consts.auth)
        .withAttribute(mechanism, "")
        .withChildren(Seq(emptyCData)),
    Sasl.Auth()
        .withMechanism(plain) ->
      Node(consts.auth)
        .withAttribute(mechanism, plain)
        .withChildren(Seq(emptyCData)),
    Sasl.Auth()
        .withMechanism(plain)
        .withData(data) ->
      Node(consts.auth)
        .withAttribute(mechanism, plain)
        .withChildren(Seq(Node(data))),
    Sasl.Success() ->
      Node(consts.success)
        .withChildren(Seq(emptyCData)),
    Sasl.Success().withData(data) ->
      Node(consts.success)
        .withChildren(Seq(Node(data))),
    Sasl.Challenge() ->
      Node(consts.challenge)
        .withChildren(Seq(emptyCData)),
    Sasl.Challenge().withData(data) ->
      Node(consts.challenge)
        .withChildren(Seq(Node(data))),
    Sasl.Response() ->
      Node(consts.response)
        .withChildren(Seq(emptyCData)),
    Sasl.Response().withData(data) ->
      Node(consts.response)
        .withChildren(Seq(Node(data))),
    Sasl.Failure() ->
      Node(consts.failure)
        .withChildren(Seq(emptyCData)),
    Sasl.Failure().withData(data) ->
      Node(consts.failure)
        .withChildren(Seq(Node(data))),
    Sasl.Abort() ->
      Node(consts.abort)
        .withChildren(Seq(emptyCData)),
    Sasl.Abort().withData(data) ->
      Node(consts.abort)
        .withChildren(Seq(Node(data))))

  "SASL stanzas" should "have a sane equality operator" in {
    val stanzas = tests.map(_._1)
    stanzas.foreach { stanza =>
      stanzas.count(_ == stanza) should be (1)
      stanzas.count(_ != stanza) should be (stanzas.size - 1)
      stanza should be (stanza)
    }
  }
  it should "render" in
    tests.foreach { case (stanza, xml) => renderTest(stanza, xml) }

  it should "parse" in
    tests.foreach { case (stanza, xml) => parseTest(decoder)(xml, stanza) }
}
