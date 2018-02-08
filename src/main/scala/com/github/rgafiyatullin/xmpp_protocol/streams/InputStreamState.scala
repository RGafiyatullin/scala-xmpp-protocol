package com.github.rgafiyatullin.xmpp_protocol.streams

import com.github.rgafiyatullin.xml.common.{HighLevelEvent, QName}
import com.github.rgafiyatullin.xml.dom.{Node, NodeBuilder}
import com.github.rgafiyatullin.xmpp_protocol.XmppConstants
import com.github.rgafiyatullin.xmpp_protocol.stream_error.XmppStreamError

private[streams] sealed trait InputStreamState {
  protected def qn(ns: String, ln: String): QName = new QName(ns, ln)

  def eventOption: Option[StreamEvent] = None

  def handleEvent: PartialFunction[HighLevelEvent, InputStreamState]
}

private[streams] object InputStreamState {

  case class ExpectStreamOpen(
                               override val eventOption: Option[StreamEvent]
                             ) extends InputStreamState
  {
    override def handleEvent: PartialFunction[HighLevelEvent, InputStreamState] = {
      case HighLevelEvent.ElementOpen(_, _, localName, ns, streamAttributes)
        if qn(ns, localName) == XmppConstants.names.streams.stream
      =>
        val event = StreamEvent.StreamOpen(streamAttributes)
        ExpectStanza(Some(event))

      case HighLevelEvent.ProcessingInstrutcion(_, "xml", _) =>
        ExpectStreamOpen(None)

//      case HighLevelEvent.ElementClose(_, _, localName, ns)
//        if qn(ns, localName) == XmppConstants.names.streams.stream
//      =>
//        val event = StreamEvent.StreamClose()
//        StreamClosed(event)

      case _ =>
        StreamError(XmppStreamError.InvalidXml())
    }
  }


  case class ExpectStanza(
                           override val eventOption: Option[StreamEvent]
                         ) extends InputStreamState
  {
    override def handleEvent: PartialFunction[HighLevelEvent, InputStreamState] = {
      case HighLevelEvent.Whitespace(_, _) =>
        copy(eventOption = None)

      case HighLevelEvent.ElementOpen(_, _, localName, ns, _)
        if qn(ns, localName) == XmppConstants.names.streams.stream
      =>
        StreamError(XmppStreamError.InvalidXml())

      case selfClosing: HighLevelEvent.ElementSelfClosing =>
        val stanza = NodeBuilder
          .empty.in(selfClosing)
          .nodeOption.get

        ExpectStanza(Some(StreamEvent.Stanza(stanza)))

      case elementOpen: HighLevelEvent.ElementOpen =>
        val builder = NodeBuilder.empty.in(elementOpen)
        BuildingStanza(builder)

      case HighLevelEvent.ElementClose(_, _, localName, ns)
        if qn(ns, localName) == XmppConstants.names.streams.stream
      =>
        StreamClosed(StreamEvent.StreamClose())

      case hle: HighLevelEvent =>
        StreamError(XmppStreamError.InvalidXml())
    }
  }


  case class BuildingStanza(
                             stanzaBuilder: NodeBuilder
                           ) extends InputStreamState
  {
    override def handleEvent: PartialFunction[HighLevelEvent, InputStreamState] = {
      case HighLevelEvent.Whitespace(_, _) => this

      case hle: HighLevelEvent =>
        val nextStanzaBuilder = stanzaBuilder.in(hle)
        nextStanzaBuilder.nodeOption match {
          case None =>
            copy(stanzaBuilder = nextStanzaBuilder)

          case Some(stanza: Node) =>
            val event = StreamEvent.Stanza(stanza)
            ExpectStanza(Some(event))

          case Some(nonElement) =>
            StreamError(XmppStreamError.InvalidXml())
        }
    }
  }


  // Terminal states

  case class StreamClosed(event: StreamEvent.StreamClose) extends InputStreamState {
    override def eventOption: Option[StreamEvent] = Some(event)
    override def handleEvent: PartialFunction[HighLevelEvent, InputStreamState] = { case _ => this }
  }

  case class StreamError(xmppStreamError: XmppStreamError) extends InputStreamState {
    override def eventOption: Option[StreamEvent] = Some(StreamEvent.StreamError(xmppStreamError))

    override def handleEvent: PartialFunction[HighLevelEvent, InputStreamState] = {
      case _ => this
    }
  }

}
