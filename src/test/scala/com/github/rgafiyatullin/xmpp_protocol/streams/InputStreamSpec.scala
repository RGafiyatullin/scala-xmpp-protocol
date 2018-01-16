package com.github.rgafiyatullin.xmpp_protocol.streams

import com.github.rgafiyatullin.xml.common
import com.github.rgafiyatullin.xml.common.{Attribute, HighLevelEvent, Position, QName}
import com.github.rgafiyatullin.xml.dom.Element
import com.github.rgafiyatullin.xmpp_protocol.XmppConstants
import com.github.rgafiyatullin.xmpp_protocol.stream_error.XmppStreamError
import org.scalatest.{FlatSpec, Matchers}

class InputStreamSpec extends FlatSpec with Matchers {
  val ep = Position.withoutPosition
  "An empty stream" should "return None initially" in {
    val is0 = InputStream.empty
    is0.out should be (None, is0)
  }

  it should "return StreamOpen event" in {
    val is0 = InputStream.empty.in(HighLevelEvent.ElementOpen(ep, "stream", "stream", "http://etherx.jabber.org/streams", Seq(
      Attribute.NsImport("", "jabber:client"),
      Attribute.NsImport("stream", "http://etherx.jabber.org/streams")
    )))
    val (event, is1) = is0.out
    event should be (Some(StreamEvent.StreamOpen(Seq(
      Attribute.NsImport("", "jabber:client"),
      Attribute.NsImport("stream", "http://etherx.jabber.org/streams")
    ))))
  }

//  it should "return StreamOpen event when there are newlines inside openning tag" in {
//    val is0 = InputStream.empty.in(
//      "<stream:stream\n\txmlns='jabber:client'\n   xmlns:stream='http://etherx.jabber.org/streams'>")
//    val (event, is1) = is0.out
//    event should be (Some(StreamEvent.StreamOpen(Seq(
//      Attribute.NsImport("", "jabber:client"),
//      Attribute.NsImport("stream", "http://etherx.jabber.org/streams")
//    ))))
//  }

//  it should "return StreamOpen event when fed in two pieces" in {
//    val piece1 = "<stream:stream xmlns='jabber:"
//    val piece2 = "client' xmlns:stream='http://etherx.jabber.org/streams'>"
//
//    val is0 = InputStream.empty.in(piece1)
//    val (none, is1) = is0.out
//    none should be (None)
//
//    val is2 = is1.in(piece2)
//    val (some, is3) = is2.out
//    some should be (Some(StreamEvent.StreamOpen(Seq(
//      Attribute.NsImport("", "jabber:client"),
//      Attribute.NsImport("stream", "http://etherx.jabber.org/streams")
//    ))))
//  }

  "An open stream" should "trigger stanza-events" in {
    val hles = Seq(
      HighLevelEvent.ElementOpen(ep, "stream", "stream", "http://etherx.jabber.org/streams", Seq(
        Attribute.NsImport("", "jabber:client"),
        Attribute.NsImport("stream", "http://etherx.jabber.org/streams")
      )),

      HighLevelEvent.ElementOpen(ep, "", "features", "http://etherx.jabber.org/streams", Seq()),
      HighLevelEvent.ElementSelfClosing(ep, "", "feature", "test", Seq(Attribute.NsImport("", "test"))),
      HighLevelEvent.ElementClose(ep, "", "features", "http://etherx.jabber.org/streams"),
      HighLevelEvent.ElementSelfClosing(ep, "", "presence", "jabber:client", Seq())
    )
    val is0 = hles.foldLeft(InputStream.empty)(_.in(_))

    val (streamOpen, is1) = is0.out
    streamOpen should be (Some(StreamEvent.StreamOpen(Seq(
      Attribute.NsImport("", "jabber:client"),
      Attribute.NsImport("stream", "http://etherx.jabber.org/streams")
    ))))
    val (stanzaStreamFeatures, is2) = is1.out
    stanzaStreamFeatures should be (Some(
        StreamEvent.Stanza(
          Element(
            XmppConstants.names.streams.features,
            Seq(), Seq(
              Element(QName("test", "feature"), Seq(), Seq())
            )))))
    val (stanzaPresence, is3) = is2.out
    stanzaPresence should be (Some(
      StreamEvent.Stanza(
        Element(
          XmppConstants.names.jabber.client.presence,
          Seq(), Seq()))))
    val (none, is4) = is3.out
    none should be (None)
  }

  it should "trigger stream error upon unexpected stream-reopen" in {
    val hles = Seq(
      HighLevelEvent.ElementOpen(ep, "stream", "stream", "http://etherx.jabber.org/streams", Seq(
        Attribute.NsImport("", "jabber:client"),
        Attribute.NsImport("stream", "http://etherx.jabber.org/streams")
      )),
      HighLevelEvent.ElementOpen(ep, "stream", "stream", "http://etherx.jabber.org/streams", Seq(
        Attribute.NsImport("", "jabber:client"),
        Attribute.NsImport("stream", "http://etherx.jabber.org/streams")
      ))
    )
    val is0 = hles.foldLeft(InputStream.empty)(_.in(_))
    val (streamOpen, is1) = is0.out
    streamOpen should be (Some(StreamEvent.StreamOpen(Seq(
      Attribute.NsImport("", "jabber:client"),
      Attribute.NsImport("stream", "http://etherx.jabber.org/streams")
    ))))
  val (localErrorInvalidXml, is2) = is1.out
    localErrorInvalidXml should be (Some(StreamEvent.StreamError(XmppStreamError.InvalidXml())))
  }

  it should "not trigger stream error upon expected stream-reopen" in {
    val hles = Seq(
      HighLevelEvent.ElementOpen(ep, "stream", "stream", "http://etherx.jabber.org/streams", Seq(
        Attribute.NsImport("", "jabber:client"),
        Attribute.NsImport("stream", "http://etherx.jabber.org/streams")
      )),
      HighLevelEvent.ElementOpen(ep, "stream", "stream", "http://etherx.jabber.org/streams", Seq(
        Attribute.NsImport("", "jabber:client"),
        Attribute.NsImport("stream", "http://etherx.jabber.org/streams")
      ))
    )
    val is0 = hles.foldLeft(InputStream.empty)(_.in(_).expectStreamOpen)
    val (streamOpen, is1) = is0.out
    streamOpen should be (Some(StreamEvent.StreamOpen(Seq(
      Attribute.NsImport("", "jabber:client"),
      Attribute.NsImport("stream", "http://etherx.jabber.org/streams")
    ))))
    val (streamOpenAgain, is2) = is1.out
    streamOpenAgain should be (Some(StreamEvent.StreamOpen(Seq(
      Attribute.NsImport("", "jabber:client"),
      Attribute.NsImport("stream", "http://etherx.jabber.org/streams")
    ))))
  }

  it should "result in stream-open and stream-closed events" in {
    val hles = Seq(
      HighLevelEvent.ElementOpen(ep, "stream", "stream", "http://etherx.jabber.org/streams", Seq(
        Attribute.NsImport("", "jabber:client"),
        Attribute.NsImport("stream", "http://etherx.jabber.org/streams")
      )),
      HighLevelEvent.ElementClose(ep, "stream", "stream", "http://etherx.jabber.org/streams")
    )
    val is0 = hles.foldLeft(InputStream.empty)(_.in(_))

    val (streamOpen, is1) = is0.out
    streamOpen should be (Some(StreamEvent.StreamOpen(Seq(
      Attribute.NsImport("", "jabber:client"),
      Attribute.NsImport("stream", "http://etherx.jabber.org/streams")
    ))))
    val (streamClosed, is2) = is1.out
    streamClosed should be (Some(StreamEvent.StreamClose()))
  }

  it should "result in local-error upon receiving non-stream open when ExpectStreamOpen" in {
    val is0 = InputStream.empty.in(
      HighLevelEvent.ElementSelfClosing(ep, "", "stanza", "jabber:client", Seq()))
    val (localError, is1) = is0.out
    localError should be (Some(StreamEvent.StreamError(XmppStreamError.InvalidXml())))
  }

  it should "result in local-error upon receiving CData on stanza-level" in {
    val hles = Seq(
      HighLevelEvent.ElementOpen(ep, "stream", "stream", "http://etherx.jabber.org/streams", Seq(
        Attribute.NsImport("", "jabber:client"),
        Attribute.NsImport("stream", "http://etherx.jabber.org/streams")
      )),
      HighLevelEvent.CData(ep, "cdata")
    )
    val is0 = hles.foldLeft(InputStream.empty)(_.in(_))
    val (streamOpen, is1) = is0.out
    streamOpen should be (Some(StreamEvent.StreamOpen(Seq(
      Attribute.NsImport("", "jabber:client"),
      Attribute.NsImport("stream", "http://etherx.jabber.org/streams")
    ))))
    val (localError, is2) = is1.out
    localError should be (Some(StreamEvent.StreamError(XmppStreamError.InvalidXml())))
  }

  it should "successfully parse <?xml version='1.0'>" in {
    val hles = Seq(
      HighLevelEvent.ProcessingInstrutcion(ep, "xml", "version='1.0'"),
      HighLevelEvent.ElementOpen(ep, "streams", "stream", "http://etherx.jabber.org/streams", Seq(
        Attribute.NsImport("streams", "http://etherx.jabber.org/streams"),
        Attribute.NsImport("", "jabber:client")
      )),
      HighLevelEvent.ElementOpen(ep, "streams", "features", "http://etherx.jabber.org/streams", Seq()),
      HighLevelEvent.ElementSelfClosing(ep, "", "router", "http://wargaming.net/xmpp/router-service", Seq()),
      HighLevelEvent.ElementClose(ep, "streams", "features", "http://etherx.jabber.org/streams")
    )
    val is0 = hles.foldLeft(InputStream.empty)(_.in(_))

//    val is0 = InputStream.empty.in(
//      """<?xml version='1.0'?><streams:stream xmlns:streams='http://etherx.jabber.org/streams' xmlns='jabber:client'>
//        |<streams:features><router xmlns='http://wargaming.net/xmpp/router-service'/></streams:features>
//        |<iq id='register-ruleset' type='error'><error><service-unavailable xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/></error></iq>
//      """.stripMargin)
    val (streamOpen, is1) = is0.out
    streamOpen should be (Some(StreamEvent.StreamOpen(Seq(
      Attribute.NsImport("streams", "http://etherx.jabber.org/streams"),
      Attribute.NsImport("", "jabber:client")
    ))))
  }

  it should "successfully parse <stream xmlns='http://etherx.jabber.org/streams'></stream>" in {
    val hles = Seq(
      HighLevelEvent.ElementOpen(ep, "", "stream", "http://etherx.jabber.org/streams", Seq(
        Attribute.NsImport("", "http://etherx.jabber.org/streams")
      )),
      HighLevelEvent.ElementClose(ep, "", "stream", "http://etherx.jabber.org/streams")
    )
    val is0 = hles.foldLeft(InputStream.empty)(_.in(_))
    val (events, is1) = is0.outAll

    events should be (Seq(
      StreamEvent.StreamOpen(Seq(Attribute.NsImport("", "http://etherx.jabber.org/streams"))),
      StreamEvent.StreamClose()
    ))
  }
}
