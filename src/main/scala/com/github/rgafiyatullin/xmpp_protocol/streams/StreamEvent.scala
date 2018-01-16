package com.github.rgafiyatullin.xmpp_protocol.streams

import com.github.rgafiyatullin.xml.common.Attribute
import com.github.rgafiyatullin.xml.dom.Node
import com.github.rgafiyatullin.xmpp_protocol.stream_error.XmppStreamError

sealed trait StreamEvent

object StreamEvent {
  case class StreamOpen(attributes: Seq[Attribute]) extends StreamEvent

  case class Stanza(element: Node) extends StreamEvent

  case class StreamClose() extends StreamEvent

  case class StreamError(xmppStreamError: XmppStreamError) extends StreamEvent
}
