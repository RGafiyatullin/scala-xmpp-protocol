package com.github.rgafiyatullin.xmpp_protocol.stanzas.presence

import com.github.rgafiyatullin.xmpp_protocol.stanzas.StanzaType

sealed trait PresenceType extends StanzaType[PresenceType] {
  final override def toString: String =
    PresenceType.toStringMap(this)

  final override def error: PresenceType =
    PresenceType.Error
}

object PresenceType {
  private val toStringMap: Map[PresenceType, String] =
    Seq(
      Error -> "error",
      Available -> "available",
      Unavailable -> "unavailable",
      Subscribe -> "subscribe",
      Unsubscribe -> "unsubscribe",
      Subscribed -> "subscribed",
      Unsubscribed -> "unsubscribed",
      Probe -> "probe"
    ).toMap

  private val fromStringMap: Map[String, PresenceType] =
    toStringMap.map(_.swap)

  def fromString(string: String): Option[PresenceType] =
    fromStringMap.get(string)

  def decode: StanzaType.Decoder[PresenceType] =
    fromStringMap

  case object Error extends PresenceType
  case object Probe extends PresenceType

  sealed trait Availability extends PresenceType
  case object Available extends Availability
  case object Unavailable extends Availability

  sealed trait Subscription extends PresenceType
  case object Subscribe extends Subscription
  case object Unsubscribe extends Subscription
  case object Subscribed extends Subscription
  case object Unsubscribed extends Subscription


}
