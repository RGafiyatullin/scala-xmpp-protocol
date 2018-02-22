package com.github.rgafiyatullin.xmpp_protocol.stanza_error

import com.github.rgafiyatullin.xml.common.QName
import com.github.rgafiyatullin.xml.dom.{CData, Node}
import com.github.rgafiyatullin.xml.dom_query.Implicits._
import com.github.rgafiyatullin.xmpp_protocol.XmppConstants
import com.github.rgafiyatullin.xmpp_protocol.error_cause.CauseToXml

sealed trait XmppStanzaError extends Exception with CauseToXml {
  def definedCondition: String

  def errorType: XmppStanzaErrorType
  def withErrorType(t: XmppStanzaErrorType): XmppStanzaError

  def reason: Option[Throwable]
  def withReason(r: Throwable): XmppStanzaError
  def withReason(ro: Option[Throwable]): XmppStanzaError


  override def getCause: Throwable = reason.orNull

  def text: Option[String]
  def withText(t: String): XmppStanzaError
  def withText(to: Option[String]): XmppStanzaError

  override def toString: String =
    text match {
      case None =>
        "XmppStanzaError(%s): %s".format(definedCondition, reason)
      case Some(textDefined) =>
        "XmppStanzaError(%s, \"%s\"): %s".format(definedCondition, textDefined, reason)
    }

  def toXml: Node = toXml(dumpCauses = false)

  def toXml(dumpCauses: Boolean): Node = {
    val causeOption = causesToXml(dumpCauses)
    val textOption =
      text.map(t =>
        Node(XmppConstants.names.urn.ietf.params.xmlNs.xmppStanzas.text)
          .withChildren(Seq(CData(t))))

    Node(XmppConstants.names.jabber.client.error)
      .withAttribute("type", errorType.toString)
      .withChildren(
        Seq(Node(QName(XmppConstants.names.urn.ietf.params.xmlNs.xmppStanzas.ns, definedCondition))) ++
        textOption.toSeq ++
        causeOption.toSeq)
  }
}

sealed trait XmppStanzaErrorBase[T <: XmppStanzaErrorBase[T]] extends XmppStanzaError {
  type Internals = XmppStanzaError.Internals

  val internals: Internals
  def withInternals(i: Internals): T

  override def errorType: XmppStanzaErrorType = internals.stanzaErrorType
  override def withErrorType(t: XmppStanzaErrorType): T = withInternals(internals.copy(stanzaErrorType = t))

  override def reason: Option[Throwable] = internals.reasonOption
  override def withReason(r: Throwable): T = withReason(Some(r))
  override def withReason(ro: Option[Throwable]): T = withInternals(internals.copy(reasonOption = ro))

  override def text: Option[String] = internals.textOption
  override def withText(t: String): T = withText(Some(t))
  override def withText(to: Option[String]): T = withInternals(internals.copy(textOption = to))
}

object XmppStanzaError {
  final case class Internals(
    stanzaErrorType: XmppStanzaErrorType,
    reasonOption: Option[Throwable] = None,
    textOption: Option[String] = None)

  def fromStanza(stanzaNode: Node): Option[XmppStanzaError] = {
    val jc = XmppConstants.names.jabber.client
    (stanzaNode select jc.error).headOption.flatMap(fromXml)
  }

  def fromXml(errorNode: Node): Option[XmppStanzaError] = {
    val xs = XmppConstants.names.urn.ietf.params.xmlNs.xmppStanzas
    if (errorNode.qName != XmppConstants.names.jabber.client.error)
      None
    else for {
      conditionNode <- errorNode.children.headOption if conditionNode.qName.ns == XmppConstants.names.urn.ietf.params.xmlNs.xmppStanzas.ns
      stanzaError <- fromDefinedCondition.lift(conditionNode.qName.localName)
      stanzaErrorType = errorNode.attribute("type").flatMap(XmppStanzaErrorType.fromString.lift).getOrElse(stanzaError.errorType)
      maybeText = (errorNode select xs.text).headOption.map(_.text)
    }
      yield stanzaError
        .withErrorType(stanzaErrorType)
        .withText(maybeText)
  }

  object conditions {
    val badRequest = "bad-request"
    val conflict = "conflict"
    val featureNotImplemented = "feature-not-implemented"
    val forbidden = "forbidden"
    val gone = "gone"
    val internalServerError = "internal-server-error"
    val itemNotFound = "item-not-found"
    val jidMalformed = "jid-malformed"
    val notAcceptable = "not-acceptable"
    val notAllowed = "not-allowed"
    val notAuthorized = "not-authorized"
    val policyViolation = "policy-violation"
    val recipientUnavailable = "recipient-unavailable"
    val redirect = "redirect"
    val registrationRequired = "registration-required"
    val remoteServerNotFound = "remote-server-not-found"
    val remoteServerTimeout = "remote-server-timeout"
    val resourceConstraint = "resource-constraint"
    val serviceUnavailable = "service-unavailable"
    val subscriptionRequired = "subscription-required"
    val undefinedCondition = "undefined-condition"
    val unexpectedRequest = "unexpected-request"
  }

  val fromDefinedCondition: PartialFunction[String, XmppStanzaError] = {
    case conditions.badRequest => BadRequest()
    case conditions.conflict => Conflict()
    case conditions.featureNotImplemented => FeatureNotImplemented()
    case conditions.forbidden => Forbidden()
    case conditions.gone => Gone()
    case conditions.internalServerError => InternalServerError()
    case conditions.itemNotFound => ItemNotFound()
    case conditions.jidMalformed => JidMalformed()
    case conditions.notAcceptable => NotAcceptable()
    case conditions.notAllowed => NotAllowed()
    case conditions.notAuthorized => NotAuthorized()
    case conditions.policyViolation => PolicyViolation()
    case conditions.recipientUnavailable => RecipientUnavailable()
    case conditions.redirect => Redirect()
    case conditions.registrationRequired => RegistrationRequired()
    case conditions.remoteServerNotFound => RemoteServerNotFound()
    case conditions.remoteServerTimeout => RemoteServerTimeout()
    case conditions.resourceConstraint => ResourceConstraint()
    case conditions.serviceUnavailable => ServiceUnavailable()
    case conditions.subscriptionRequired => SubscriptionRequired()
    case conditions.undefinedCondition => UndefinedCondition()
    case conditions.unexpectedRequest => UnexpectedRequest()
  }


  /**
    * Represents stanza level error with defined condition "bad-requiest"
    *
    * The sender has sent a stanza containing XML that does not conform to the appropriate schema or that cannot
    * be processed (e.g., an IQ stanza that includes an unrecognized value of the 'type' attribute, or an element that
    * is qualified by a recognized namespace but that violates the defined syntax for the element);
    * the associated error type SHOULD be "modify".
    *
    */
  final case class BadRequest(internals: Internals = Internals(XmppStanzaErrorType.Modify))
    extends XmppStanzaErrorBase[BadRequest]
  {
    def definedCondition = XmppStanzaError.conditions.badRequest
    override def withInternals(i: Internals) = copy(internals = i)
  }


  /**
    * Represents stanza level error with defined condition "conflict"
    *
    * Access cannot be granted because an existing resource exists with the same name or address;
    * the associated error type SHOULD be "cancel".
    */
  final case class Conflict(internals: Internals = Internals(XmppStanzaErrorType.Cancel))
    extends XmppStanzaErrorBase[Conflict]
  {
    def definedCondition = XmppStanzaError.conditions.conflict
    override def withInternals(i: Internals) = copy(internals = i)
  }


  /**
    * Represents stanza level error with defined condition "feature-not-implemented"
    *
    * The feature represented in the XML stanza is not implemented by the intended recipient or an intermediate server
    * and therefore the stanza cannot be processed (e.g., the entity understands the namespace but does not recognize
    * the element name); the associated error type SHOULD be "cancel" or "modify".
    */
  final case class FeatureNotImplemented(internals: Internals = Internals(XmppStanzaErrorType.Cancel))
    extends XmppStanzaErrorBase[FeatureNotImplemented]
  {
    def definedCondition = XmppStanzaError.conditions.featureNotImplemented
    override def withInternals(i: Internals) = copy(internals = i)
  }


  /**
    * Represents stanza level error with defined condition "forbidden"
    *
    * The requesting entity does not possess the necessary permissions to perform an action that only certain authorized
    * roles or individuals are allowed to complete (i.e., it typically relates to authorization rather than
    * authentication); the associated error type SHOULD be "auth".
    */
  final case class Forbidden(internals: Internals = Internals(XmppStanzaErrorType.Auth))
    extends XmppStanzaErrorBase[Forbidden]
  {
    def definedCondition = XmppStanzaError.conditions.forbidden
    override def withInternals(i: Internals) = copy(internals = i)
  }


  /**
    * Represents stanza level error with defined condition "gone"
    *
    * The recipient or server can no longer be contacted at this address, typically on a permanent basis
    * (as opposed to the "redirect" error condition, which is used for temporary addressing failures);
    * the associated error type SHOULD be "cancel" and the error stanza SHOULD include a new address
    * (if available) as the XML character data of the "gone" element (which MUST be a Uniform Resource Identifier [URI]
    * or Internationalized Resource Identifier [IRI] at which the entity can be contacted,
    * typically an XMPP IRI as specified in [XMPP‑URI]).
    */
  final case class Gone(internals: Internals = Internals(XmppStanzaErrorType.Cancel))
    extends XmppStanzaErrorBase[Gone] {
    def definedCondition = XmppStanzaError.conditions.gone
    override def withInternals(i: Internals) = copy(internals = i)
  }


  /**
    * Represents stanza level error with defined condition "internal-server-error"
    *
    * The server has experienced a misconfiguration or other internal error that prevents it from processing the stanza;
    * the associated error type SHOULD be "cancel".
    *
    */
  final case class InternalServerError(internals: Internals = Internals(XmppStanzaErrorType.Cancel))
    extends XmppStanzaErrorBase[InternalServerError] {
    def definedCondition = XmppStanzaError.conditions.internalServerError
    override def withInternals(i: Internals) = copy(internals = i)
  }


  /**
    * Represents stanza level error with defined condition "item-not-found"
    *
    * The addressed JID or item requested cannot be found; the associated error type SHOULD be "cancel".
    *
    */
  final case class ItemNotFound(internals: Internals = Internals(XmppStanzaErrorType.Cancel))
    extends XmppStanzaErrorBase[ItemNotFound] {
    def definedCondition = XmppStanzaError.conditions.itemNotFound
    override def withInternals(i: Internals) = copy(internals = i)
  }


  /**
    * Represents stanza level error with defined condition "jid-malformed"
    *
    * The sending entity has provided (e.g., during resource binding) or communicated
    * (e.g., in the 'to' address of a stanza) an XMPP address or aspect thereof that violates the rules defined
    * in [XMPP‑ADDR]; the associated error type SHOULD be "modify".
    *
    */
  final case class JidMalformed(internals: Internals = Internals(XmppStanzaErrorType.Modify))
    extends XmppStanzaErrorBase[JidMalformed] {
    def definedCondition = XmppStanzaError.conditions.jidMalformed
    override def withInternals(i: Internals) = copy(internals = i)
  }


  /**
    * Represents stanza level error with defined condition "not-acceptable"
    *
    * The recipient or server understands the request but cannot process it because the request does not meet criteria
    * defined by the recipient or server (e.g., a request to subscribe to information that does not simultaneously
    * include configuration parameters needed by the recipient); the associated error type SHOULD be "modify".
    *
    */
  final case class NotAcceptable(internals: Internals = Internals(XmppStanzaErrorType.Modify))
    extends XmppStanzaErrorBase[NotAcceptable] {
    def definedCondition = XmppStanzaError.conditions.notAcceptable
    override def withInternals(i: Internals) = copy(internals = i)
  }


  /**
    * Represents stanza level error with defined condition "not-allowed"
    *
    * The recipient or server does not allow any entity to perform the action
    * (e.g., sending to entities at a blacklisted domain); the associated error type SHOULD be "cancel".
    *
    */
  final case class NotAllowed(internals: Internals = Internals(XmppStanzaErrorType.Cancel))
    extends XmppStanzaErrorBase[NotAllowed] {
    def definedCondition = XmppStanzaError.conditions.notAllowed
    override def withInternals(i: Internals) = copy(internals = i)
  }


  /**
    * Represents stanza level error with defined condition "not-authorized"
    *
    * The sender needs to provide credentials before being allowed to perform the action,
    * or has provided improper credentials (the name "not-authorized", which was borrowed from the "401 Unauthorized"
    * error of [HTTP], might lead the reader to think that this condition relates to authorization,
    * but instead it is typically used in relation to authentication); the associated error type SHOULD be "auth".
    *
    */
  final case class NotAuthorized(internals: Internals = Internals(XmppStanzaErrorType.Auth))
    extends XmppStanzaErrorBase[NotAuthorized] {
    def definedCondition = XmppStanzaError.conditions.notAuthorized
    override def withInternals(i: Internals) = copy(internals = i)
  }


  /**
    * Represents stanza level error with defined condition "policy-violation"
    *
    * The entity has violated some local service policy
    * (e.g., a message contains words that are prohibited by the service) and the server MAY choose to specify the policy
    * in the "text" element or in an application-specific condition element;
    * the associated error type SHOULD be "modify" or "wait" depending on the policy being violated.
    *
    */
  final case class PolicyViolation(internals: Internals = Internals(XmppStanzaErrorType.Modify))
    extends XmppStanzaErrorBase[PolicyViolation] {
    def definedCondition = XmppStanzaError.conditions.policyViolation
    override def withInternals(i: Internals) = copy(internals = i)
  }


  /**
    * Represents stanza level error with defined condition "recipient-unavailable"
    *
    * The intended recipient is temporarily unavailable, undergoing maintenance, etc.;
    * the associated error type SHOULD be "wait".
    *
    */
  final case class RecipientUnavailable(internals: Internals = Internals(XmppStanzaErrorType.Wait))
    extends XmppStanzaErrorBase[RecipientUnavailable] {
    def definedCondition = XmppStanzaError.conditions.recipientUnavailable
    override def withInternals(i: Internals) = copy(internals = i)
  }


  /**
    * Represents stanza level error with defined condition "redirect"
    *
    * The intended recipient is temporarily unavailable, undergoing maintenance, etc.;
    * the associated error type SHOULD be "wait".
    *
    */
  final case class Redirect(internals: Internals = Internals(XmppStanzaErrorType.Wait))
    extends XmppStanzaErrorBase[Redirect] {
    def definedCondition = XmppStanzaError.conditions.redirect
    override def withInternals(i: Internals) = copy(internals = i)
  }


  /**
    * Represents stanza level error with defined condition "registration-required"
    *
    * The requesting entity is not authorized to access the requested service because prior registration is necessary
    * (examples of prior registration include members-only rooms in XMPP multi-user chat [XEP‑0045] and gateways to
    * non-XMPP instant messaging services, which traditionally required registration in order to use the gateway
    * [XEP‑0100]); the associated error type SHOULD be "auth".
    *
    */
  final case class RegistrationRequired(internals: Internals = Internals(XmppStanzaErrorType.Auth))
    extends XmppStanzaErrorBase[RegistrationRequired] {
    def definedCondition = XmppStanzaError.conditions.registrationRequired
    override def withInternals(i: Internals) = copy(internals = i)
  }


  /**
    * Represents stanza level error with defined condition "remote-server-not-found"
    *
    * A remote server or service specified as part or all of the JID of the intended recipient does not exist or
    * cannot be resolved (e.g., there is no _xmpp-server._tcp DNS SRV record, the A or AAAA fallback resolution fails,
    * or A/AAAA lookups succeed but there is no response on the IANA-registered port 5269);
    * the associated error type SHOULD be "cancel".
    *
    */
  final case class RemoteServerNotFound(internals: Internals = Internals(XmppStanzaErrorType.Cancel))
    extends XmppStanzaErrorBase[RemoteServerNotFound] {
    def definedCondition = XmppStanzaError.conditions.remoteServerNotFound
    override def withInternals(i: Internals) = copy(internals = i)
  }

  /**
    * Represents stanza level error with defined condition "remote-server-timeout"
    *
    * A remote server or service specified as part or all of the JID of the intended recipient
    * (or needed to fulfill a request) was resolved but communications could not be established within a
    * reasonable amount of time (e.g., an XML stream cannot be established at the resolved IP address and port,
    * or an XML stream can be established but stream negotiation fails because of problems with TLS, SASL,
    * Server Dialback, etc.); the associated error type SHOULD be "wait" (unless the error is of a more permanent
    * nature, e.g., the remote server is found but it cannot be authenticated or it violates security policies).
    *
    */
  final case class RemoteServerTimeout(internals: Internals = Internals(XmppStanzaErrorType.Wait))
    extends XmppStanzaErrorBase[RemoteServerTimeout] {
    def definedCondition = XmppStanzaError.conditions.remoteServerTimeout
    override def withInternals(i: Internals) = copy(internals = i)
  }


  /**
    * Represents stanza level error with defined condition "resource-constraint"
    *
    * The server or recipient is busy or lacks the system resources necessary to service the request;
    * the associated error type SHOULD be "wait".
    *
    */
  final case class ResourceConstraint(internals: Internals = Internals(XmppStanzaErrorType.Wait))
    extends XmppStanzaErrorBase[ResourceConstraint] {
    def definedCondition = XmppStanzaError.conditions.resourceConstraint
    override def withInternals(i: Internals) = copy(internals = i)
  }

  /**
    * Represents stanza level error with defined condition "???"
    *
    * The server or recipient does not currently provide the requested service;
    * the associated error type SHOULD be "cancel".
    *
    */
  final case class ServiceUnavailable(internals: Internals = Internals(XmppStanzaErrorType.Cancel))
    extends XmppStanzaErrorBase[ServiceUnavailable] {
    def definedCondition = XmppStanzaError.conditions.serviceUnavailable
    override def withInternals(i: Internals) = copy(internals = i)
  }

  /**
    * Represents stanza level error with defined condition "subscription-required"
    *
    * The requesting entity is not authorized to access the requested service because a prior subscription is
    * necessary (examples of prior subscription include authorization to receive presence information as defined
    * in [XMPP‑IM] and opt-in data feeds for XMPP publish-subscribe as defined in [XEP‑0060]);
    * the associated error type SHOULD be "auth".
    *
    */
  final case class SubscriptionRequired(internals: Internals = Internals(XmppStanzaErrorType.Auth))
    extends XmppStanzaErrorBase[SubscriptionRequired] {
    def definedCondition = XmppStanzaError.conditions.subscriptionRequired
    override def withInternals(i: Internals) = copy(internals = i)
  }

  /**
    * Represents stanza level error with defined condition "undefined-condition"
    *
    * The error condition is not one of those defined by the other conditions in this list;
    * any error type can be associated with this condition, and it SHOULD NOT be used except
    * in conjunction with an application-specific condition.
    *
    */
  final case class UndefinedCondition(internals: Internals = Internals(XmppStanzaErrorType.Cancel))
    extends XmppStanzaErrorBase[UndefinedCondition] {
    def definedCondition = XmppStanzaError.conditions.undefinedCondition
    override def withInternals(i: Internals) = copy(internals = i)
  }

  /**
    * Represents stanza level error with defined condition "unexpected-request"
    *
    * The recipient or server understood the request but was not expecting it at this time
    * (e.g., the request was out of order); the associated error type SHOULD be "wait" or "modify".
    *
    */
  final case class UnexpectedRequest(internals: Internals = Internals(XmppStanzaErrorType.Wait))
    extends XmppStanzaErrorBase[UnexpectedRequest] {
    def definedCondition = XmppStanzaError.conditions.unexpectedRequest
    override def withInternals(i: Internals) = copy(internals = i)
  }

}
