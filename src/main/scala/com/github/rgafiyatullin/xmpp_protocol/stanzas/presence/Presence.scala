package com.github.rgafiyatullin.xmpp_protocol.stanzas.presence

import com.github.rgafiyatullin.xml.common.Attribute
import com.github.rgafiyatullin.xml.dom.Node
import com.github.rgafiyatullin.xmpp_protocol.XmppConstants
import com.github.rgafiyatullin.xmpp_protocol.stanza_error.XmppStanzaError
import com.github.rgafiyatullin.xmpp_protocol.stanzas.{Stanza, StanzaUtil}

sealed trait Presence
  extends Stanza
    with Stanza.HasIDOption[Presence]
    with Stanza.HasAttributes[Presence]
{
  def presenceType: PresenceType
  protected def renderedChildNodes: Seq[Node]

  final override def toXml: Node =
    Node(XmppConstants.names.jabber.client.presence)
      .withAttributes(attributes.map(Attribute.Unprefixed.tupled).toSeq)
      .withAttribute("type", presenceType.toString)
      .withAttribute("id", idOption)
      .withChildren(renderedChildNodes)
}

object Presence {
  private object util extends StanzaUtil

  sealed trait Request
    extends Presence
      with Stanza.HasError[Error]
      with Stanza.HasIDOption[Request]
      with Stanza.HasChildren[Request]
      with Stanza.HasAttributes[Request]

  sealed abstract class PresenceFamily[PT <: PresenceType] {
    sealed trait P
      extends Request
        with Stanza.HasError[Error]
        with Stanza.HasIDOption[P]
        with Stanza.HasAttributes[P]
    {
      final override protected def renderedChildNodes: Seq[Node] = children

      override def error(xmppStanzaError: XmppStanzaError): Error =
        Presence.error(xmppStanzaError)

      override def withIdOption(newIdOption: Option[String]): P =
        new Wrapper(this) {
          override def idOption: Option[String] =
            newIdOption }

      override def withAttribute(name: String, value: String): P =
        new Wrapper(this) {
          override def attributes: Map[String, String] =
            inner.attributes + (name -> value) }

      override def withoutAttribute(name: String): P =
        new Wrapper(this) {
          override def attributes: Map[String, String] =
            inner.attributes - name }

      override def withChildren(newChildren: Seq[Node]): Request =
        new Wrapper(this) {
          override def children: Seq[Node] = newChildren }
    }

    protected final class Root(val presenceType: PresenceType) extends P {
      override def children: Seq[Node] = Seq.empty
      override def attributes: Map[String, String] = Map.empty
      override def idOption: Option[String] = None
    }
    protected abstract class Wrapper(protected val inner: P) extends P {
      override def presenceType: PresenceType = inner.presenceType
      override def children: Seq[Node] = inner.children
      override def attributes: Map[String, String] = inner.attributes
      override def idOption: Option[String] = inner.idOption
    }
  }

  sealed trait PresenceFamilyWithApply1[PT <: PresenceType] extends PresenceFamily[PT] {
    def apply(presenceType: PT): P = new Root(presenceType)
  }
  sealed trait PresenceFamilyWithApply0[PT <: PresenceType] extends PresenceFamily[PT] {
    protected val theOnlyPresenceType: PT
    def apply(): P = new Root(theOnlyPresenceType)
  }

  object probe extends PresenceFamilyWithApply0[PresenceType.Probe.type] {
    override protected val theOnlyPresenceType: PresenceType.Probe.type = PresenceType.Probe
  }
  object availability extends PresenceFamilyWithApply1[PresenceType.Availability]
  object subscription extends PresenceFamilyWithApply1[PresenceType.Subscription]

  def error(xmppStanzaError: XmppStanzaError): Error =
    ErrorRoot(xmppStanzaError)

  sealed trait Error extends Presence {
    final override def presenceType: PresenceType = PresenceType.Error

    final override protected def renderedChildNodes: Seq[Node] =
      Seq(xmppStanzaError.toXml)

    def requestOption: Option[Presence.Request]
    def xmppStanzaError: XmppStanzaError

    def withRequestOption(newRequestOption: Option[Presence.Request]): Error =
      new ErrorWrapper(this) {
        override def requestOption: Option[Request] =
          newRequestOption }

    def withXmppStanzaError(newXmppStanzaError: XmppStanzaError): Error =
      new ErrorWrapper(this) {
        override def xmppStanzaError: XmppStanzaError =
          newXmppStanzaError }

    override def withIdOption(newIdOption: Option[String]): Presence =
      new ErrorWrapper(this) {
        override def idOption: Option[String] =
          newIdOption }

    override def withAttribute(name: String, value: String): Presence =
      new ErrorWrapper(this) {
        override def attributes: Map[String, String] =
          inner.attributes + (name -> value) }

    override def withoutAttribute(name: String): Presence =
      new ErrorWrapper(this) {
        override def attributes: Map[String, String] =
          inner.attributes - name }
  }

  private final case class ErrorRoot(xmppStanzaError: XmppStanzaError) extends Error {
    override def requestOption: Option[Request] = None
    override def idOption: Option[String] = None
    override def attributes: Map[String, String] = Map.empty
  }

  private abstract class ErrorWrapper(protected val inner: Error) extends Error {
    override def xmppStanzaError: XmppStanzaError = inner.xmppStanzaError
    override def requestOption: Option[Request] = inner.requestOption
    override def idOption: Option[String] = inner.idOption
    override def attributes: Map[String, String] = inner.attributes
  }
}
