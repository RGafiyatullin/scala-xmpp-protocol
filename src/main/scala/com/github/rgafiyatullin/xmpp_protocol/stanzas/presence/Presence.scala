package com.github.rgafiyatullin.xmpp_protocol.stanzas.presence

import com.github.rgafiyatullin.xml.common.Attribute
import com.github.rgafiyatullin.xml.dom.Node
import com.github.rgafiyatullin.xmpp_protocol.XmppConstants
import com.github.rgafiyatullin.xmpp_protocol.stanza_error.XmppStanzaError
import com.github.rgafiyatullin.xmpp_protocol.stanzas.{Stanza, StanzaUtil}

import scala.reflect.ClassTag
import scala.util.{Success, Try}

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

      override def withChildren(newChildren: Seq[Node]): P =
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
  sealed trait PresenceFamilyWithApply0[PT <: PresenceType] extends PresenceFamilyWithApply1[PT] {
    protected val theOnlyPresenceType: PT
    def apply(): P = apply(theOnlyPresenceType)
  }

  object probe extends PresenceFamilyWithApply0[PresenceType.Probe.type] {
    override protected val theOnlyPresenceType: PresenceType.Probe.type = PresenceType.Probe
  }
  object availability extends PresenceFamilyWithApply1[PresenceType.Availability]
  object subscription extends PresenceFamilyWithApply1[PresenceType.Subscription]

  def error(xmppStanzaError: XmppStanzaError): Error =
    ErrorRoot(xmppStanzaError)

  sealed trait Error
    extends Presence
      with Stanza.HasIDOption[Error]
      with Stanza.HasAttributes[Error]
  {
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

    override def withIdOption(newIdOption: Option[String]): Error =
      new ErrorWrapper(this) {
        override def idOption: Option[String] =
          newIdOption }

    override def withAttribute(name: String, value: String): Error =
      new ErrorWrapper(this) {
        override def attributes: Map[String, String] =
          inner.attributes + (name -> value) }

    override def withoutAttribute(name: String): Error =
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

  def decodeError: Stanza.Decoder[Presence.Error] =
    new Stanza.Decoder[Presence.Error] {
      override def isDefinedAt(node: Node): Boolean =
        node.qName == XmppConstants.names.jabber.client.presence &&
          node.attribute("type").contains(PresenceType.Error.toString)

      override def apply(node: Node): Try[Presence.Error] = Try {
        val xse = XmppStanzaError.fromStanza(node)
          .getOrElse(throw XmppStanzaError.BadRequest().withText("Failed to parse XSE"))
        util.copyAttributesFromNode(
          Presence.error(xse), node,
          Set("id", "type"))
            .withIdOption(node.attribute("id"))
      }
    }

  private object decoderInternals {
    import scala.reflect.classTag

    val ctAvailability: ClassTag[PresenceType.Availability] = classTag[PresenceType.Availability]
    val ctSubscription: ClassTag[PresenceType.Subscription] = classTag[PresenceType.Subscription]
    val ctProbe: ClassTag[PresenceType.Probe.type] = classTag[PresenceType.Probe.type]
  }

  private def decodeNonError
    [PT <: PresenceType]
    (pFamily: PresenceFamilyWithApply1[PT],
     defaultTypeOption: Option[PT] = None)
    (implicit classTag: ClassTag[PT])
  : Stanza.Decoder[pFamily.P] =
    new Stanza.Decoder[pFamily.P] {
      private def presenceTypeOption(node: Node): Option[PT] =
        node.attribute("type")
          .map(PresenceType.decode)
          .orElse(defaultTypeOption)
          .map(_.asInstanceOf[PT])

      private def isDefinedAtType(node: Node): Boolean =
        Try(presenceTypeOption(node)) match {
          case Success(Some(_: PT)) => true
          case _ => false
        }

      override def isDefinedAt(node: Node): Boolean =
        node.qName == XmppConstants.names.jabber.client.presence &&
          isDefinedAtType(node)

      override def apply(node: Node): Try[pFamily.P] = Try {
        util.copyAttributesFromNode(
          pFamily(presenceTypeOption(node).get),
          node, Set("id", "type"))
            .withChildren(node.children)
            .withIdOption(node.attribute("id"))
      }
    }

  def decodeAvailability: Stanza.Decoder[availability.P] =
    decodeNonError(availability, Some(PresenceType.Available))(decoderInternals.ctAvailability)

  def decodeSubscription: Stanza.Decoder[subscription.P] =
    decodeNonError(subscription)(decoderInternals.ctSubscription)

  def decodeProbe: Stanza.Decoder[probe.P] =
    decodeNonError(probe)(decoderInternals.ctProbe)

  def decodeRequest: Stanza.Decoder[Presence.Request] =
    decodeAvailability orElse decodeSubscription orElse decodeProbe

  def decode: Stanza.Decoder[Presence] =
    decodeRequest orElse decodeError

}
