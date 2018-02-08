package com.github.rgafiyatullin.xmpp_protocol.stanzas

trait StanzaType[+Self <: StanzaType[Self]] extends StanzaType.Untyped {
  override def error: Self
}

object StanzaType {
  sealed trait Untyped {
    final def isError: Boolean =
      error == this

    def error: Untyped
  }


  type Decoder[+ST <: StanzaType[ST]] = PartialFunction[String, ST]
}
