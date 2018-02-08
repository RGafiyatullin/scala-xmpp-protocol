package com.github.rgafiyatullin.xmpp_protocol.stanzas.message

import com.github.rgafiyatullin.xmpp_protocol.stanzas.StanzaType

sealed trait MessageType extends StanzaType[MessageType] {
  final override def error: MessageType = MessageType.Error

  final override def toString: String =
    MessageType.toStringMap(this)
}

object MessageType {
  private val toStringMap: Map[MessageType, String] =
    Seq(
      Error -> "error",
      Normal -> "normal",
      Chat -> "chat",
      Groupchat -> "groupchat",
      Headline -> "headline"
    ).toMap

  private val fromStringMap: Map[String, MessageType] =
    toStringMap.map(_.swap)

  def fromString(string: String): Option[MessageType] =
    fromStringMap.get(string)

  def decode: StanzaType.Decoder[MessageType] =
    fromStringMap

  case object Error extends MessageType
  case object Normal extends MessageType
  case object Chat extends MessageType
  case object Groupchat extends MessageType
  case object Headline extends MessageType
}
