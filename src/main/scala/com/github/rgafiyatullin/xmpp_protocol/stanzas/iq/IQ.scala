package com.github.rgafiyatullin.xmpp_protocol.stanzas.iq

import java.util.UUID

import com.github.rgafiyatullin.xml.common.Attribute
import com.github.rgafiyatullin.xml.dom.Node
import com.github.rgafiyatullin.xmpp_protocol.XmppConstants
import com.github.rgafiyatullin.xmpp_protocol.stanza_error.XmppStanzaError
import com.github.rgafiyatullin.xmpp_protocol.stanzas.{Stanza, StanzaUtil}

import scala.util.Try

sealed trait IQ
  extends Stanza
    with Stanza.HasID[IQ]
    with Stanza.HasAttributes[IQ]
    with Stanza.HasAddresses[IQ]
{
  def iqType: IQType
  def attributes: Map[String, String]

  protected def renderedChildNodes: Seq[Node]

  def childNodes: Seq[Node]

  final override def toXml: Node =
    Node(XmppConstants.names.jabber.client.iq)
      .withAttributes(attributes.map(Attribute.Unprefixed.tupled).toSeq)
      .withAttribute("id", id)
      .withAttribute("type", iqType.toString)
      .withChildren(renderedChildNodes)
}

object IQ {
  private object util extends StanzaUtil

  def request
    (id: String,
     iqType: IQType.Request,
     body: Node)
  : Request =
    RequestRoot(id, iqType, body)

  def request(iqType: IQType.Request, body: Node): Request =
    request(UUID.randomUUID().toString, iqType, body)

  def request(body: Node): Request =
    request(IQType.Get, body)

  def result(id: String): Result =
    ResultRoot(id)

  def error(id: String, xmppStanzaError: XmppStanzaError): Error =
    ErrorRoot(id, xmppStanzaError)

  def decodeRequest: Stanza.Decoder[Request] =
    new Stanza.Decoder[Request] {
      override def isDefinedAt(node: Node): Boolean =
        node.qName == XmppConstants.names.jabber.client.iq &&
          node.attribute("type").exists(t => t == IQType.Get.toString || t == IQType.Set.toString)

      override def apply(node: Node): Try[Request] = {
        require(isDefinedAt(node))
        Try {
          val id = util.id(node)
          val iqType = node.attribute("type").get match {
            case "get" => IQType.Get
            case "set" => IQType.Set
          }
          val body = node.children.headOption
            .getOrElse(throw XmppStanzaError.BadRequest().withText("request-IQ must have a child-node"))

          val request1 = request(id, iqType, body)
          val request2 = util.copyAttributesFromNode(
            request1, node, Set("id", "type"))

          request2
        }
      }
    }

  def decodeResponse: Stanza.Decoder[Response] =
    new Stanza.Decoder[Response] {
      override def isDefinedAt(node: Node): Boolean =
        node.qName == XmppConstants.names.jabber.client.iq &&
          node.attribute("type").exists(t => t == IQType.Result.toString || t == IQType.Error.toString)

      override def apply(node: Node): Try[Response] = {
        require(isDefinedAt(node))
        Try {
          val id = util.id(node)
          val typeStr = node.attribute("type")
            .getOrElse(throw XmppStanzaError.BadRequest().withText("type-attribute missing"))

          if (typeStr == IQType.Result.toString) {
            val result1 = IQ.result(id)
            val result2 = node.children
              .headOption.fold(result1)(result1.withBody)
            val result3 = util.copyAttributesFromNode(
              result2, node, Set("id", "type"))

            result3
          } else /* if (typeStr == IQType.Error.toString) */ {
            val xse = XmppStanzaError.fromStanza(node)
              .getOrElse(throw XmppStanzaError.BadRequest().withText("Failed to parse XSE"))
            val error1 = IQ.error(id, xse)
            val error2 = util.copyAttributesFromNode(
              error1, node, Set("id", "type"))

            error2
          }
        }
      }
    }

  sealed trait Request
    extends IQ
      with Stanza.HasID[Request]
      with Stanza.HasBody[Request]
      with Stanza.HasAttributes[Request]
      with Stanza.HasError[IQ.Error]
      with Stanza.HasAddresses[Request]
  {
    override protected def renderedChildNodes: Seq[Node] = Seq(body)

    override def iqType: IQType.Request
    def body: Node

    def result: IQ.Result =
      IQ.result(id)

    override def error(xmppStanzaError: XmppStanzaError): IQ.Error =
      IQ
        .error(id, xmppStanzaError)
        .withRequest(this)
        .withToOption(fromOption)
        .withFromOption(toOption)

    def withBody(newBody: Node): Request =
      new RequestWrapper(this) {
        override def body: Node = newBody
      }

    def withAttribute(name: String, value: String): Request =
      new RequestWrapper(this) {
        override def attributes: Map[String, String] =
          inner.attributes + (name -> value) }

    def withoutAttribute(name: String): Request =
      new RequestWrapper(this) {
        override def attributes: Map[String, String] =
          inner.attributes - name }

    override def withId(newId: String): Request =
      new RequestWrapper(this) {
        override def id: String = newId }

    def withType(newType: IQType.Request): Request =
      new RequestWrapper(this) {
        override def iqType: IQType.Request = newType }
  }

  private final case class RequestRoot
    (id: String,
     iqType: IQType.Request,
     body: Node) extends Request
  {
    override def attributes: Map[String, String] = Map.empty
    override def childNodes: Seq[Node] = Seq.empty
  }

  private abstract class RequestWrapper(protected val inner: Request) extends Request {
    override def id: String = inner.id
    override def iqType: IQType.Request = inner.iqType
    override def body: Node = inner.body
    override def childNodes: Seq[Node] = Seq.empty
    override def attributes: Map[String, String] = inner.attributes
  }

  sealed trait Response
    extends IQ
      with Stanza.HasID[Response]
      with Stanza.HasAttributes[Response]
      with Stanza.HasAddresses[Response]

  sealed trait Result
    extends Response
      with Stanza.HasID[Result]
      with Stanza.HasBodyOption[Result]
      with Stanza.HasAttributes[Result]
      with Stanza.HasAddresses[Result]
  {
    override protected def renderedChildNodes: Seq[Node] = bodyOption.toSeq

    final override def iqType: IQType.Result.type = IQType.Result

    override def withId(newId: String): Result =
      new ResultWrapper(this) {
        override def id: String = newId }

    override def withBodyOption(newBodyOption: Option[Node]): Result =
      new ResultWrapper(this) {
        override def bodyOption: Option[Node] = newBodyOption }

    def withAttribute(name: String, value: String): Result =
      new ResultWrapper(this) {
        override def attributes: Map[String, String] =
          inner.attributes + (name -> value) }

    def withoutAttribute(name: String): Result =
      new ResultWrapper(this) {
        override def attributes: Map[String, String] =
          inner.attributes - name }
  }

  private final case class ResultRoot(id: String) extends Result {
    override def bodyOption: Option[Node] = None
    override def childNodes: Seq[Node] = Seq.empty
    override def attributes: Map[String, String] = Map.empty
  }

  private abstract class ResultWrapper(protected val inner: Result) extends Result {
    override def id: String = inner.id
    override def attributes: Map[String, String] = inner.attributes
    override def bodyOption: Option[Node] = inner.bodyOption
    override def childNodes: Seq[Node] = inner.childNodes
  }

  sealed trait Error
    extends Response
      with Stanza.HasID[Error]
      with Stanza.HasAttributes[Error]
      with Stanza.HasAddresses[Error]
  {
    override protected def renderedChildNodes: Seq[Node] = Seq(reason.toXml)

    final override def iqType: IQType.Error.type = IQType.Error
    def reason: XmppStanzaError

    def requestOption: Option[IQ.Request]

    def withRequest(request: IQ.Request): IQ.Error =
      withRequestOption(Some(request))
    def withoutRequest: IQ.Error =
      withRequestOption(None)

    def withRequestOption(newRequestOption: Option[IQ.Request]): IQ.Error =
      new ErrorWrapper(this) {
        override def requestOption: Option[Request] = newRequestOption
      }

    override def withAttribute(name: String, value: String): Error =
      new ErrorWrapper(this) {
        override def attributes: Map[String, String] =
          inner.attributes + (name -> value) }

    override def withoutAttribute(name: String): Error =
      new ErrorWrapper(this) {
        override def attributes: Map[String, String] =
          inner.attributes - name }

    def withReason(newXmppStanzaError: XmppStanzaError): Error =
      new ErrorWrapper(this) {
        override def reason: XmppStanzaError = newXmppStanzaError }

    override def withId(newId: String): Error =
      new ErrorWrapper(this) {
        override def id: String = newId }
  }

  private final case class ErrorRoot(id: String, reason: XmppStanzaError) extends Error {
    override def attributes: Map[String, String] = Map.empty
    override def childNodes: Seq[Node] = Seq.empty
    override def requestOption: Option[Request] = None
  }
  private abstract class ErrorWrapper(protected val inner: Error) extends Error {
    override def requestOption: Option[Request] = inner.requestOption
    override def reason: XmppStanzaError = inner.reason
    override def id: String = inner.id
    override def attributes: Map[String, String] = inner.attributes
    override def childNodes: Seq[Node] = inner.childNodes
  }
}