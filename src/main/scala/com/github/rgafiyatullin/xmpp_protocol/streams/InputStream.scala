package com.github.rgafiyatullin.xmpp_protocol.streams

import com.github.rgafiyatullin.xml.common.HighLevelEvent
import com.github.rgafiyatullin.xmpp_protocol.stream_error.XmppStreamError
import scala.collection.immutable.Queue

object InputStream {
  def empty: InputStream =
    InputStream(
      InputStreamState.ExpectStreamOpen(None))
}


case class InputStream(state: InputStreamState, output: Queue[StreamEvent] = Queue.empty) {
  def in(hle: HighLevelEvent): InputStream = {
    val state1 = state.handleEvent.applyOrElse(hle, unexpectedParserEvent)
    state1.eventOption match {
      case None =>
        copy(state = state1)
      case Some(event) =>
        copy(state = state1, output = output.enqueue(event))
    }
  }

  def out: (Option[StreamEvent], InputStream) =
    (output.headOption, copy(output = output.drop(1)))

  def outAll: (Seq[StreamEvent], InputStream) =
    (output, copy(output = Queue.empty))

  def expectStreamOpen: InputStream =
    copy(state = InputStreamState.ExpectStreamOpen(None))

  private def unexpectedParserEvent(highLevelEvent: HighLevelEvent): InputStreamState =
    InputStreamState.StreamError(XmppStreamError.InternalServerError())

}


