package com.github.rgafiyatullin.xmpp_protocol.stanzas.iq

import com.github.rgafiyatullin.xmpp_protocol.stanzas.StanzaType

sealed trait IQType extends StanzaType[IQType] {
  final override def toString: String =
    IQType.toStringMap(this)

  final override def error: IQType = IQType.Error
}

object IQType {
  private val toStringMap: Map[IQType, String] =
    Seq(
      Get -> "get",
      Set -> "set",
      Result -> "result",
      Error -> "error"
    ).toMap
  private val fromStringMap: Map[String, IQType] =
    toStringMap.map(_.swap)

  def fromString(string: String): Option[IQType] =
    fromStringMap.get(string)

  def decode: StanzaType.Decoder[IQType] =
    fromStringMap

  sealed trait Request extends IQType
  case object Get extends Request
  case object Set extends Request
  case object Result extends IQType
  case object Error extends IQType
}
