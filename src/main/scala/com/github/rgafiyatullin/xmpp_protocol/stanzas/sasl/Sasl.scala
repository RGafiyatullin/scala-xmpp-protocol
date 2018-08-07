package com.github.rgafiyatullin.xmpp_protocol.stanzas.sasl

import java.util.Base64

import com.github.rgafiyatullin.xml.common.{Attribute, QName}
import com.github.rgafiyatullin.xml.dom.Node
import com.github.rgafiyatullin.xmpp_protocol.XmppConstants
import com.github.rgafiyatullin.xmpp_protocol.stanzas.{Stanza, sasl}

import scala.util.Try

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

  def decode: Stanza.Decoder[Sasl] =
    Auth.decode orElse
    Success.decode orElse
    Challenge.decode orElse
    Response.decode orElse
    Failure.decode orElse
    Abort.decode

  sealed trait Auth extends Sasl {
    final override protected def fqn: QName = consts.auth

    def mechanism: String
    def data: String

    def withMechanism(m: String): Auth =
      new Auth.Wrapper(this) {
        override def mechanism: String = m
      }

    def withData(d: String): Auth =
      new sasl.Sasl.Auth.Wrapper(this) {
        override def data: String = d
      }

    final override protected def attributes: Map[String, String] = Map("mechanism" -> mechanism)
    final override protected def children: Seq[Node] = Seq(Node(data))
  }
  object Auth {
    def apply(): Auth = Root
    def apply(mechanism: String): Auth = apply().withMechanism(mechanism)
    def apply(mechanism: String, data: String): Auth = apply(mechanism).withData(data)

    private final case object Root extends Auth {
      override def data: String = ""
      override def mechanism: String = ""
    }

    private class Wrapper(protected val inner: Auth) extends Auth {
      override def mechanism: String = inner.mechanism
      override def data: String = inner.data
    }

    def decode: Stanza.Decoder[Auth] =
      createDecoder(consts.auth, xml =>
        Auth()
          .withMechanism(xml.attribute("mechanism").getOrElse(""))
          .withData(xml.text))

  }

  sealed trait Success extends Sasl {
    final override protected def fqn: QName = consts.success

    def data: String

    def withData(d: String): Success =
      new Success.Wrapper(this) {
        override def data: String = d
      }

    override protected def attributes: Map[String, String] = Map.empty
    override protected def children: Seq[Node] = Seq(Node(data))
  }
  object Success {
    def apply(): Success = Root

    private case object Root extends Success {
      override def data: String = ""
    }
    private class Wrapper(protected val inner: Success) extends Success {
      override def data: String = inner.data
    }

    def decode: Stanza.Decoder[Success] =
      createDecoder(consts.success, xml =>
        Success().withData(xml.text))
  }

  sealed trait Challenge extends Sasl {
    final override protected def fqn: QName = consts.challenge
    def data: String
    def withData(d: String): Challenge =
      new Challenge.Wrapper(this) {
        override def data: String = d
      }

    override protected def attributes: Map[String, String] = Map.empty
    override protected def children: Seq[Node] = Seq(Node(data))
  }
  object Challenge {
    def apply(): Challenge = Root

    private case object Root extends Challenge {
      override def data: String = ""
    }
    private class Wrapper(protected val inner: Challenge) extends Challenge {
      override def data: String = inner.data
    }

    def decode: Stanza.Decoder[Challenge] =
      createDecoder(consts.challenge, xml =>
        Challenge().withData(xml.text))
  }

  sealed trait Response extends Sasl {
    final override protected def fqn: QName = consts.response
    def data: String
    def withData(d: String): Response =
      new Response.Wrapper(this){
        override def data: String = d
      }

    override protected def attributes: Map[String, String] = Map.empty
    override protected def children: Seq[Node] = Seq(Node(data))
  }
  object Response {
    def apply(): Response = Root

    private case object Root extends Response {
      override def data: String = ""
    }
    private class Wrapper(protected val inner: Response) extends Response {
      override def data: String = inner.data
    }

    def decode: Stanza.Decoder[Response] =
      createDecoder(consts.response, xml =>
        Response().withData(xml.text))
  }

  sealed trait Failure extends Sasl {
    final override protected def fqn: QName = consts.failure
    def data: String
    def withData(d: String): Failure =
      new Failure.Wrapper(this){
        override def data: String = d
      }

    override protected def attributes: Map[String, String] = Map.empty
    override protected def children: Seq[Node] = Seq(Node(data))
  }
  object Failure {
    def apply(): Failure = Root

    private case object Root extends Failure {
      override def data: String = ""
    }
    private class Wrapper(protected val inner: Failure) extends Failure {
      override def data: String = inner.data
    }

    def decode: Stanza.Decoder[Failure] =
      createDecoder(consts.failure, xml =>
        Failure().withData(xml.text))
  }

  sealed trait Abort extends Sasl {
    final override protected def fqn: QName = consts.abort
    def data: String
    def withData(d: String): Abort =
      new Abort.Wrapper(this){
        override def data: String = d
      }

    override protected def attributes: Map[String, String] = Map.empty
    override protected def children: Seq[Node] = Seq(Node(data))
  }
  object Abort {
    def apply(): Abort = Root

    private case object Root extends Abort {
      override def data: String = ""
    }
    private class Wrapper(protected val inner: Abort) extends Abort {
      override def data: String = inner.data
    }

    def decode: Stanza.Decoder[Abort] =
      createDecoder(consts.abort, xml =>
        Abort().withData(xml.text))
  }


  private def createDecoder[S <: Sasl](qn: QName, fromXml: Node => S): Stanza.Decoder[S] =
    new Stanza.Decoder[S] {
      override def isDefinedAt(xml: Node): Boolean =
        xml.qName == qn

      override def apply(xml: Node): Try[S] = {
        // it's safe to rely on it: otherwise it would be !isDefinedAt(thisValue)
        assert(xml.qName == qn)
        Try(fromXml(xml))
      }
    }

}
