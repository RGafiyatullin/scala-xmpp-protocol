package com.github.rgafiyatullin.xmpp_protocol.streams

import com.github.rgafiyatullin.xml.common.Attribute
import com.github.rgafiyatullin.xml.stream_writer.high_level_writer.HighLevelWriter
import com.github.rgafiyatullin.xmpp_protocol.XmppConstants
import org.scalatest.{FlatSpec, Matchers}

class OutputStreamSpec extends FlatSpec with Matchers {
  "An OutputStream" should "process StreamOpen-event" in {
    val os0 = OutputStream.empty
    val os1 = os0.in(StreamEvent.StreamOpen(Seq()))
    val (streamOpen,  os2) = os1.out
    val streamOpenStrings = streamOpen.foldLeft(HighLevelWriter.empty)(_.in(_)).out._1

    streamOpenStrings.mkString should be ("<stream xmlns='http://etherx.jabber.org/streams'>")
  }

  it should "process StreamOpen-event with NS-hint" in {
    val os0 = OutputStream.empty
    val os1 = os0.in(StreamEvent.StreamOpen(Seq(Attribute.NsImport("streams", XmppConstants.names.streams.ns))))
    val (streamOpen,  os2) = os1.out
    val streamOpenStrings = streamOpen.foldLeft(HighLevelWriter.empty)(_.in(_)).out._1

    streamOpenStrings.mkString should be ("<streams:stream xmlns:streams='http://etherx.jabber.org/streams'>")
  }

//  it should "process Stanza-event" in {
//    val os0 = streamOpen
//    val os1 = os0.in(StreamEvent.Stanza(???))
//  }
}
