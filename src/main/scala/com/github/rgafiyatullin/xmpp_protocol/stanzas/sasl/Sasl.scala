package com.github.rgafiyatullin.xmpp_protocol.stanzas.sasl

import java.util.Base64

import com.github.rgafiyatullin.xml.common.{Attribute, QName}
import com.github.rgafiyatullin.xml.dom.{CData, Node}
import com.github.rgafiyatullin.xmpp_protocol.XmppConstants
import com.github.rgafiyatullin.xmpp_protocol.stanzas.Stanza

sealed trait Sasl extends Stanza {
  protected def fqn: QName
  protected def attributes: Map[String, String]
  protected def children: Seq[Node]

  override def toXml: Node =
    Node(fqn)
      .withAttributes(attributes.map(Attribute.Unprefixed.tupled).toSeq)
      .withChildren(children)
}

object Sasl {
  import XmppConstants.names.urn.ietf.params.xmlNs.{xmppSasl => consts}

  sealed trait Auth extends Sasl {
    def mechanism: String
    def responseOption: Option[Array[Byte]]

    final override protected def fqn: QName =
      consts.auth

    final override protected def attributes: Map[String, String] =
      Seq("mechanism" -> mechanism).toMap

    final override protected def children: Seq[Node] =
      responseOption match {
        case None => Seq.empty
        case Some(Array()) => Seq(CData("="))
        case Some(nonEmptyArray) => Seq(CData(Base64.getEncoder.encodeToString(nonEmptyArray)))
      }
  }

  final case class AuthWithResponse(mechanism: String, response: Array[Byte]) extends Auth {
    override def responseOption: Option[Array[Byte]] = Some(response)
  }

  final case class AuthNoResponse(mechanism: String) extends Auth {
    override def responseOption: Option[Array[Byte]] = None
  }

  sealed trait Challenge extends Sasl
  sealed trait Response extends Sasl
  sealed trait Failure extends Sasl
  sealed trait Abort extends Sasl
}
