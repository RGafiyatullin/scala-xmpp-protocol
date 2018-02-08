package com.github.rgafiyatullin.xmpp_protocol.stanzas

import com.github.rgafiyatullin.xml.common.Attribute
import com.github.rgafiyatullin.xml.dom.Node
import com.github.rgafiyatullin.xmpp_protocol.stanza_error.XmppStanzaError

trait StanzaUtil {
  def id(node: Node): String =
    node.attribute("id").getOrElse(throw XmppStanzaError.BadRequest().withText("id-attribute missing"))

  def copyAttributesFromNode[A <: Stanza.HasAttributes[A]]
  (stanza0: A,
   node: Node,
   attrsToExclude: Set[String])
  : A =
    node.attributes
      .collect {
        case Attribute.Unprefixed(name, value) => name -> value
      }
      .filter(pair => !attrsToExclude.contains(pair._1))
      .foldLeft(stanza0) {
        case (r, (name, value)) =>
          r.withAttribute(name, value)
      }
}
