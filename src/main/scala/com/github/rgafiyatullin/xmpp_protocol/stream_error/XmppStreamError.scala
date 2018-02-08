package com.github.rgafiyatullin.xmpp_protocol.stream_error

import com.github.rgafiyatullin.xml.common.QName
import com.github.rgafiyatullin.xml.dom.{CData, Element, Node}
import com.github.rgafiyatullin.xml.dom_query.Implicits._
import com.github.rgafiyatullin.xml.dom_query.Predicate
import com.github.rgafiyatullin.xmpp_protocol.XmppConstants

sealed trait XmppStreamError extends Exception {
  def reason: Option[Throwable]
  def withReason(r: Throwable): XmppStreamError
  def withReason(ro: Option[Throwable]): XmppStreamError


  def text: Option[String]
  def withText(t: String): XmppStreamError
  def withText(to: Option[String]): XmppStreamError

  def definedCondition: String

  override def toString: String =
    "XmppStreamError(%s): %s".format(definedCondition, reason)

  def toXml: Node =
    Element(
      XmppConstants.names.streams.error,
      Seq(),
      Seq(
        Element(
          QName(XmppConstants.names.urn.ietf.params.xmlNs.xmppStreams.ns, definedCondition),
          Seq(),
          text.map {
            str =>
              Element(XmppConstants.names.urn.ietf.params.xmlNs.xmppStreams.text, Seq(), Seq(CData(str)))
          }.toSeq)))
}

sealed trait XmppStreamErrorBase[T <: XmppStreamErrorBase[T]] extends XmppStreamError {
  type Internals = XmppStreamError.Internals
  val internals: Internals
  def withInternals(i: Internals): T

  override def reason: Option[Throwable] = internals.reasonOption
  override def withReason(r: Throwable): T = withReason(Some(r))
  override def withReason(ro: Option[Throwable]): T = withInternals(internals.copy(reasonOption = ro))

  override def text: Option[String] = internals.textOption
  override def withText(t: String): T = withText(Some(t))
  override def withText(to: Option[String]): T = withInternals(internals.copy(textOption = to))
}

object XmppStreamError {
  def fromStanza(stanzaNode: Node): Option[XmppStreamError] =
    stanzaNode.qName match {
      case XmppConstants.names.streams.error =>
        for {
          definedCondition <- (stanzaNode select Predicate.NsIs(XmppConstants.names.urn.ietf.params.xmlNs.xmppStreams.ns)).headOption
          streamError <- XmppStreamError.fromDefinedCondition.lift(definedCondition.qName.localName)
          textOption = (stanzaNode select
            Predicate.NsIs(XmppConstants.names.urn.ietf.params.xmlNs.xmppStreams.ns) /
              XmppConstants.names.urn.ietf.params.xmlNs.xmppStreams.text)
            .headOption
            .map(_.text)
        }
          yield streamError.withText(textOption)

      case _ =>
        None
    }

  final case class Internals(reasonOption: Option[Throwable] = None, textOption: Option[String] = None)


  object conditions {
    val badFormat = "bad-format"
    val badNamespacePrefix = "bad-namespace-prefix"
    val conflict = "conflict"
    val connectionTimeout = "connection-timeout"
    val hostGone = "host-gone"
    val hostUnknown = "host-unknown"
    val improperAddressing = "improper-addressing"
    val internalServerError = "internal-server-error"
    val invalidFrom = "invalid-from"
    val invalidNamespace = "invalid-namespace"
    val invalidXml = "invalid-xml"
    val notAuthorized = "not-authorized"
    val notWellFormed = "not-well-formed"
    val policyViolation = "policy-violation"
    val remoteConnectionFailed = "remote-connection-failed"
    val reset = "reset"
    val resourceConstraint = "resource-constraint"
    val restrictedXml = "restricted-xml"
    val seeOtherHost = "see-other-host"
    val systemShutdown = "system-shutdown"
    val undefinedCondition = "undefined-condition"
    val unsupportedEncoding = "unsupported-encoding"
    val unsupportedFeature = "unsupported-feature"
    val unsupportedStanzaType = "unsupported-stanza-type"
    val unsupportedVersion = "unsupported-version"
  }

  val fromDefinedCondition: PartialFunction[String, XmppStreamError] = {
    case conditions.badFormat => BadFormat()
    case conditions.badNamespacePrefix => BadNamespacePrefix()
    case conditions.conflict => Conflict()
    case conditions.connectionTimeout => ConnectionTimeout()
    case conditions.hostGone => HostGone()
    case conditions.hostUnknown => HostUnknown()
    case conditions.improperAddressing => ImproperAddressing()
    case conditions.internalServerError => InternalServerError()
    case conditions.invalidFrom => InvalidFrom()
    case conditions.invalidNamespace => InvalidNamespace()
    case conditions.invalidXml => InvalidXml()
    case conditions.notAuthorized => NotAuthorized()
    case conditions.notWellFormed => NotWellFormed()
    case conditions.policyViolation => PolicyViolation()
    case conditions.remoteConnectionFailed => RemoteConnectionFailed()
    case conditions.reset => Reset()
    case conditions.resourceConstraint => ResourceConstraint()
    case conditions.restrictedXml => RestrictedXml()
    case conditions.seeOtherHost => SeeOtherHost("") // FIXME: should we do something with that?
    case conditions.systemShutdown => SystemShutdown()
    case conditions.undefinedCondition => UndefinedCondition()
    case conditions.unsupportedEncoding => UnsupportedEncoding()
    case conditions.unsupportedFeature => UnsupportedFeature()
    case conditions.unsupportedStanzaType => UnsupportedStanzaType()
    case conditions.unsupportedVersion => UnsupportedVersion()
  }


  /**
    * RFC-6120 4.9.3.  Defined Conditions
    */


  /**
    * Represents stream level error with condition "bad-format".
    *
    * The entity has sent XML that cannot be processed.
    *
    */
  final case class BadFormat(internals: Internals = Internals()) extends XmppStreamErrorBase[BadFormat] {
    override def definedCondition: String = XmppStreamError.conditions.badFormat
    override def withInternals(i: Internals) = copy(internals = i)
  }

  /**
    * Represents stream level error with condition "bad-namespace-prefix"
    *
    * The entity has sent a namespace prefix that is unsupported,
    * or has sent no namespace prefix on an element that needs such a prefix
    *
    */
  final case class BadNamespacePrefix(internals: Internals = Internals()) extends XmppStreamErrorBase[BadNamespacePrefix] {
    override def definedCondition: String = XmppStreamError.conditions.badNamespacePrefix
    override def withInternals(i: Internals) = copy(internals = i)
  }

  /**
    * Represents stream level error with condition "bad-namespace-prefix"
    *
    * The server either (1) is closing the existing stream for this entity because a new stream has been initiated that conflicts with the existing stream,
    * or (2) is refusing a new stream for this entity because allowing the new stream would conflict with an existing stream
    * (e.g., because the server allows only a certain number of connections from the same IP address or allows only one server-to-server stream for a given domain pair as a way
    * of helping to ensure in-order processing as described under Section 10.1).
    *
    */
  final case class Conflict(internals: Internals = Internals()) extends XmppStreamErrorBase[Conflict] {
    override def definedCondition: String = XmppStreamError.conditions.conflict
    override def withInternals(i: Internals) = copy(internals = i)
  }


  /**
    * Represents stream level error with condition "connection-timeout"
    *
    * One party is closing the stream because it has reason to believe that the other party has permanently lost the ability to communicate over the stream.
    * The lack of ability to communicate can be discovered using various methods, such as whitespace keepalives as specified under Section 4.4,
    * XMPP-level pings as defined in [XEP‑0199], and XMPP Stream Management as defined in [XEP‑0198].
    *
    */
  final case class ConnectionTimeout(internals: Internals = Internals()) extends XmppStreamErrorBase[ConnectionTimeout] {
    override def definedCondition: String = XmppStreamError.conditions.connectionTimeout
    override def withInternals(i: Internals) = copy(internals = i)
  }

  /**
    * Represents stream level error with condition "host-gone"
    *
    * The value of the 'to' attribute provided in the initial stream header corresponds to an FQDN
    * that is no longer serviced by the receiving entity.
    *
    */
  final case class HostGone(internals: Internals = Internals()) extends XmppStreamErrorBase[HostGone] {
    override def definedCondition: String = XmppStreamError.conditions.hostGone
    override def withInternals(i: Internals) = copy(internals = i)
  }

  /**
    * Represents stream level error with condition "host-unknown"
    *
    * The value of the 'to' attribute provided in the initial stream header does not correspond to an FQDN
    * that is serviced by the receiving entity.
    *
    */
  final case class HostUnknown(internals: Internals = Internals()) extends XmppStreamErrorBase[HostUnknown] {
    override def definedCondition: String = XmppStreamError.conditions.hostUnknown
    override def withInternals(i: Internals) = copy(internals = i)
  }

  /**
    * Represents stream level error with condition "improper-addressing"
    *
    * A stanza sent between two servers lacks a 'to' or 'from' attribute, the 'from' or 'to' attribute has no value,
    * or the value violates the rules for XMPP addresses [XMPP‑ADDR].
    *
    */
  final case class ImproperAddressing(internals: Internals = Internals()) extends XmppStreamErrorBase[ImproperAddressing] {
    override def definedCondition: String = XmppStreamError.conditions.improperAddressing
    override def withInternals(i: Internals) = copy(internals = i)
  }

  /**
    * Represents stream level error with condition "internal-server-error"
    *
    * The server has experienced a misconfiguration or other internal error that prevents it from servicing the stream.
    *
    */
  final case class InternalServerError(internals: Internals = Internals()) extends XmppStreamErrorBase[InternalServerError] {
    override def definedCondition: String = XmppStreamError.conditions.internalServerError
    override def withInternals(i: Internals) = copy(internals = i)
  }

  /**
    * Represents stream level error with condition "invalid-from"
    *
    * The data provided in a 'from' attribute does not match an authorized JID or validated domain as negotiated (1)
    * between two servers using SASL or Server Dialback, or (2) between a client and a server via
    * SASL authentication and resource binding.
    *
    */
  final case class InvalidFrom(internals: Internals = Internals()) extends XmppStreamErrorBase[InvalidFrom] {
    override def definedCondition: String = XmppStreamError.conditions.invalidFrom
    override def withInternals(i: Internals) = copy(internals = i)
  }

  /**
    * Represents stream level error with condition "invalid-namespace"
    *
    * The stream namespace name is something other than "http://etherx.jabber.org/streams" (see Section 11.2) or
    * the content namespace declared as the default namespace is not supported
    * (e.g., something other than "jabber:client" or "jabber:server").
    *
    */
  final case class InvalidNamespace(internals: Internals = Internals()) extends XmppStreamErrorBase[InvalidNamespace] {
    override def definedCondition: String = XmppStreamError.conditions.invalidNamespace
    override def withInternals(i: Internals) = copy(internals = i)
  }

  /**
    * Represents stream level error with condition "invalid-xml"
    *
    * The entity has sent invalid XML over the stream to a server that performs validation
    *
    */
  final case class InvalidXml(internals: Internals = Internals()) extends XmppStreamErrorBase[InvalidXml] {
    override def definedCondition: String = XmppStreamError.conditions.invalidXml
    override def withInternals(i: Internals) = copy(internals = i)
  }

  /**
    * Represents stream level error with condition "not-authorized"
    *
    * The entity has attempted to send XML stanzas or other outbound data before the stream has been authenticated,
    * or otherwise is not authorized to perform an action related to stream negotiation; the receiving entity
    * MUST NOT process the offending data before sending the stream error.
    *
    */
  final case class NotAuthorized(internals: Internals = Internals()) extends XmppStreamErrorBase[NotAuthorized] {
    override def definedCondition: String = XmppStreamError.conditions.notAuthorized
    override def withInternals(i: Internals) = copy(internals = i)
  }

  /**
    * Represents stream level error with condition "not-well-formed"
    *
    * The initiating entity has sent XML that violates the well-formedness rules of [XML] or [XML‑NAMES].
    *
    */
  final case class NotWellFormed(internals: Internals = Internals()) extends XmppStreamErrorBase[NotWellFormed] {
    override def definedCondition: String = XmppStreamError.conditions.notWellFormed
    override def withInternals(i: Internals) = copy(internals = i)
  }

  /**
    * Represents stream level error with condition "policy-violation"
    *
    * The entity has violated some local service policy (e.g., a stanza exceeds a configured size limit);
    * the server MAY choose to specify the policy in the <text/> element or in an application-specific condition element.
    *
    */
  final case class PolicyViolation(internals: Internals = Internals()) extends XmppStreamErrorBase[PolicyViolation] {
    override def definedCondition: String = XmppStreamError.conditions.policyViolation
    override def withInternals(i: Internals) = copy(internals = i)
  }

  /**
    * Represents stream level error with condition "remote-connection-failed"
    *
    * The server is unable to properly connect to a remote entity that is needed for authentication or authorization
    * (e.g., in certain scenarios related to Server Dialback [XEP‑0220]); this condition is not to be used when the
    * cause of the error is within the administrative domain of the XMPP service provider, in which case the
    * "internal-server-error" condition is more appropriate.
    *
    */
  final case class RemoteConnectionFailed(internals: Internals = Internals()) extends XmppStreamErrorBase[RemoteConnectionFailed] {
    override def definedCondition: String = XmppStreamError.conditions.remoteConnectionFailed
    override def withInternals(i: Internals) = copy(internals = i)
  }

  /**
    * Represents stream level error with condition "reset"
    *
    * The server is closing the stream because it has new (typically security-critical) features to offer,
    * because the keys or certificates used to establish a secure context for the stream have expired or have been
    * revoked during the life of the stream (Section 13.7.2.3),
    * because the TLS sequence number has wrapped (Section 5.3.5), etc.
    * The reset applies to the stream and to any security context established for that stream (e.g., via TLS and SASL),
    * which means that encryption and authentication need to be negotiated again for the new stream
    * (e.g., TLS session resumption cannot be used).
    *
    */
  final case class Reset(internals: Internals = Internals()) extends XmppStreamErrorBase[Reset] {
    override def definedCondition: String = XmppStreamError.conditions.reset
    override def withInternals(i: Internals) = copy(internals = i)
  }

  /**
    * Represents stream level error with condition "resource-constraint"
    *
    * The server lacks the system resources necessary to service the stream.
    *
    */
  final case class ResourceConstraint(internals: Internals = Internals()) extends XmppStreamErrorBase[ResourceConstraint] {
    override def definedCondition: String = XmppStreamError.conditions.resourceConstraint
    override def withInternals(i: Internals) = copy(internals = i)
  }

  /**
    * Represents stream level error with condition "restricted-xml"
    *
    * The entity has attempted to send restricted XML features such as a comment,
    * processing instruction, DTD subset, or XML entity reference (see Section 11.1).
    *
    */
  final case class RestrictedXml(internals: Internals = Internals()) extends XmppStreamErrorBase[RestrictedXml] {
    override def definedCondition: String = XmppStreamError.conditions.restrictedXml
    override def withInternals(i: Internals) = copy(internals = i)
  }

  /**
    * Represents stream level error with condition "see-other-host"
    *
    * The server will not provide service to the initiating entity but is redirecting traffic to another host under
    * the administrative control of the same service provider.
    * The XML character data of the "see-other-host" element returned by the server MUST specify the alternate
    * FQDN or IP address at which to connect, which MUST be a valid domainpart or a domainpart plus port number
    * (separated by the ':' character in the form "domainpart:port").
    * If the domainpart is the same as the source domain, derived domain, or resolved IPv4 or IPv6 address to which the
    * initiating entity originally connected (differing only by the port number), then the initiating entity SHOULD
    * simply attempt to reconnect at that address.
    * (The format of an IPv6 address MUST follow [IPv6‑ADDR], which includes the enclosing the IPv6 address in
    * square brackets '[' and ']' as originally defined by [URI].) Otherwise, the initiating entity MUST resolve
    * the FQDN specified in the "see-other-host" element as described under Section 3.2.
    *
    */
  final case class SeeOtherHost(otherHost: String, internals: Internals = Internals()) extends XmppStreamErrorBase[SeeOtherHost] {
    override def definedCondition: String = XmppStreamError.conditions.seeOtherHost
    def withOtherHost(oh: String): SeeOtherHost = copy(otherHost = oh)
    override def withInternals(i: Internals) = copy(internals = i)
  }

  /**
    * Represents stream level error with condition "system-shutdown"
    *
    * The server is being shut down and all active streams are being closed.
    *
    */
  final case class SystemShutdown(internals: Internals = Internals()) extends XmppStreamErrorBase[SystemShutdown] {
    override def definedCondition: String = XmppStreamError.conditions.systemShutdown
    override def withInternals(i: Internals) = copy(internals = i)
  }

  /**
    * Represents stream level error with condition "undefined-condition"
    *
    * The error condition is not one of those defined by the other conditions in this list;
    * this error condition SHOULD NOT be used except in conjunction with an application-specific condition.
    *
    */
  final case class UndefinedCondition(internals: Internals = Internals()) extends XmppStreamErrorBase[UndefinedCondition] {
    override def definedCondition: String = XmppStreamError.conditions.undefinedCondition
    override def withInternals(i: Internals) = copy(internals = i)
  }

  /**
    * Represents stream level error with condition "unsupported-encoding"
    *
    * The initiating entity has encoded the stream in an encoding that is not supported by the server (see Section 11.6)
    * or has otherwise improperly encoded the stream (e.g., by violating the rules of the [UTF‑8] encoding).
    *
    */
  final case class UnsupportedEncoding(internals: Internals = Internals()) extends XmppStreamErrorBase[UnsupportedEncoding] {
    override def definedCondition: String = XmppStreamError.conditions.unsupportedEncoding
    override def withInternals(i: Internals) = copy(internals = i)
  }

  /**
    * Represents stream level error with condition "unsupported-feature"
    *
    * The receiving entity has advertised a mandatory-to-negotiate stream feature that the initiating entity does
    * not support, and has offered no other mandatory-to-negotiate feature alongside the unsupported feature.
    *
    */
  final case class UnsupportedFeature(internals: Internals = Internals()) extends XmppStreamErrorBase[UnsupportedFeature] {
    override def definedCondition: String = XmppStreamError.conditions.unsupportedFeature
    override def withInternals(i: Internals) = copy(internals = i)
  }

  /**
    * Represents stream level error with condition "unsupported-stanza-type"
    *
    * The initiating entity has sent a first-level child of the stream that is not supported by the server,
    * either because the receiving entity does not understand the namespace or because the receiving entity
    * does not understand the element name for the applicable namespace
    * (which might be the content namespace declared as the default namespace).
    *
    */
  final case class UnsupportedStanzaType(internals: Internals = Internals()) extends XmppStreamErrorBase[UnsupportedStanzaType] {
    override def definedCondition: String = XmppStreamError.conditions.unsupportedStanzaType
    override def withInternals(i: Internals) = copy(internals = i)
  }

  /**
    * Represents stream level error with condition "unsupported-version"
    *
    * The 'version' attribute provided by the initiating entity in the stream header specifies
    * a version of XMPP that is not supported by the server.
    *
    */
  final case class UnsupportedVersion(internals: Internals = Internals()) extends XmppStreamErrorBase[UnsupportedVersion] {
    override def definedCondition: String = XmppStreamError.conditions.unsupportedVersion
    override def withInternals(i: Internals) = copy(internals = i)
  }

}
