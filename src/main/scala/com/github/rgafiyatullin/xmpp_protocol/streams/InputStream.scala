package com.github.rgafiyatullin.xmpp_protocol.streams

import com.github.rgafiyatullin.xml.common.HighLevelEvent
import com.github.rgafiyatullin.xmpp_protocol.stream_error.XmppStreamError

import scala.collection.immutable.Queue
import scala.util.{Failure, Success, Try}

object InputStream {
  def empty: InputStream =
    InputStream(
      InputStreamState.ExpectStreamOpen(None))
}


case class InputStream(state: InputStreamState, output: Queue[Try[StreamEvent]] = Queue.empty) {
  def in(hle: HighLevelEvent): InputStream = {
    val state1 = state.handleEvent.applyOrElse(hle, unexpectedParserEvent)
    Try(state1.eventOption) match {
      case Success(None) =>
        copy(state = state1)
      case Success(Some(event)) =>
        copy(state = state1, output = output.enqueue(Success(event)))
      case Failure(reason) =>
        copy(state = state1, output = output.enqueue(Failure(reason)))
    }
  }

  def out: (Option[StreamEvent], InputStream) =
    (output.headOption.map(_.get), copy(output = output.drop(1)))

  def outAll: (Seq[StreamEvent], InputStream) =
    (output.map(_.get), copy(output = Queue.empty))

  def expectStreamOpen: InputStream =
    copy(state = InputStreamState.ExpectStreamOpen(None))

  private def unexpectedParserEvent(highLevelEvent: HighLevelEvent): InputStreamState =
    InputStreamState.StreamError(XmppStreamError.InternalServerError())

}


