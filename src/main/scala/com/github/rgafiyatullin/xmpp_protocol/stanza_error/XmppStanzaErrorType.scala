package com.github.rgafiyatullin.xmpp_protocol.stanza_error

sealed trait XmppStanzaErrorType

object XmppStanzaErrorType {
  object types {
    val auth = "auth"
    val cancel = "cancel"
    val continue = "continue"
    val modify = "modify"
    val wait_ = "wait"
  }

  case object Auth extends XmppStanzaErrorType { override val toString: String = types.auth }
  case object Cancel extends XmppStanzaErrorType { override val toString: String = types.cancel }
  case object Continue extends XmppStanzaErrorType { override val toString: String = types.continue }
  case object Modify extends XmppStanzaErrorType { override val toString: String = types.modify }
  case object Wait extends XmppStanzaErrorType { override val toString: String = types.wait_ }

  val fromString: PartialFunction[String, XmppStanzaErrorType] = {
    case "" => Cancel
    case types.auth => Auth
    case types.cancel => Cancel
    case types.continue => Continue
    case types.modify => Modify
    case types.wait_ => Wait
  }
}
