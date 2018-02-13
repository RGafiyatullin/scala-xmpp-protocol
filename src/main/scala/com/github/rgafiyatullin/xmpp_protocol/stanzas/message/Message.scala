package com.github.rgafiyatullin.xmpp_protocol.stanzas.message

import com.github.rgafiyatullin.xml.common.Attribute
import com.github.rgafiyatullin.xml.dom.Node
import com.github.rgafiyatullin.xmpp_protocol.XmppConstants
import com.github.rgafiyatullin.xmpp_protocol.stanza_error.XmppStanzaError
import com.github.rgafiyatullin.xmpp_protocol.stanzas.{Stanza, StanzaUtil}

sealed trait Message
  extends Stanza
    with Stanza.HasIDOption[Message]
    with Stanza.HasAttributes[Message]
{
  def messageType: MessageType

  protected def renderedChildNodes: Seq[Node]

  final override def toXml: Node =
    Node(XmppConstants.names.jabber.client.message)
      .withAttributes(attributes.map(Attribute.Unprefixed.tupled).toSeq)
      .withAttribute("type", messageType.toString)
      .withAttribute("id", idOption)
      .withChildren(renderedChildNodes)

}

object Message {
  private object util extends StanzaUtil

  def request(messageType: MessageType): Request = RequestRoot(messageType)
  def chat: Request = request(MessageType.Chat)
  def normal: Request = request(MessageType.Normal)
  def groupchat: Request = request(MessageType.Groupchat)
  def headline: Request = request(MessageType.Headline)

  def error(xmppStanzaError: XmppStanzaError): Error =
    ErrorRoot(xmppStanzaError)


  sealed trait Request
    extends Message
      with Stanza.HasIDOption[Request]
      with Stanza.HasChildren[Request]
      with Stanza.HasAttributes[Request]
      with Stanza.HasError[Error]
  {
    final override protected def renderedChildNodes: Seq[Node] = children

    def withMessageType(newMessageType: MessageType): Request =
      new RequestWrapper(this) {
        override def messageType: MessageType = newMessageType }

    override def error(xmppStanzaError: XmppStanzaError): Error =
      error(xmppStanzaError).withRequest(this)

    override def withAttribute(name: String, value: String): Request =
      new RequestWrapper(this) {
        override def attributes: Map[String, String] =
          inner.attributes + (name -> value) }

    override def withoutAttribute(name: String): Request =
      new RequestWrapper(this) {
        override def attributes: Map[String, String] =
          inner.attributes - name }

    override def withIdOption(newIdOption: Option[String]): Request =
      new RequestWrapper(this) {
        override def idOption: Option[String] = newIdOption }

    override def withChildren(newChildNodes: Seq[Node]): Request =
      new RequestWrapper(this) {
        override def children: Seq[Node] = newChildNodes }

  }

  private final case class RequestRoot(messageType: MessageType) extends Request {
    override def idOption: Option[String] = None
    override def attributes: Map[String, String] = Map.empty
    override def children: Seq[Node] = Seq.empty
  }
  private abstract class RequestWrapper(protected val inner: Request) extends Request {
    override def messageType: MessageType = inner.messageType
    override def children: Seq[Node] = inner.children
    override def attributes: Map[String, String] = inner.attributes
    override def idOption: Option[String] = inner.idOption
  }

  sealed trait Error
    extends Message
      with Stanza.HasIDOption[Error]
      with Stanza.HasAttributes[Error]
  {
    final override def messageType: MessageType = MessageType.Error
    final override protected def renderedChildNodes: Seq[Node] = Seq(xmppStanzaError.toXml)

    def xmppStanzaError: XmppStanzaError
    def withXmppStanzaError(newXmppStanzaError: XmppStanzaError): Error =
      new ErrorWrapper(this) {
        override def xmppStanzaError: XmppStanzaError = newXmppStanzaError
      }

    def requestOption: Option[Message.Request]
    def withRequest(request: Message.Request): Message.Error =
      withRequestOption(Some(request))
    def withoutRequest: Message.Error =
      withRequestOption(None)

    def withRequestOption(newRequestOption: Option[Message.Request]): Message.Error =
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

    def withIdOption(newIdOption: Option[String]): Error =
      new ErrorWrapper(this) {
        override def idOption: Option[String] = newIdOption
      }
  }

  final case class ErrorRoot(xmppStanzaError: XmppStanzaError) extends Error {
    override def requestOption: Option[Request] = None
    override def attributes: Map[String, String] = Map.empty
    override def idOption: Option[String] = None
  }

  private abstract class ErrorWrapper(protected val inner: Error) extends Error {
    override def idOption: Option[String] = inner.idOption
    override def requestOption: Option[Request] = inner.requestOption
    override def attributes: Map[String, String] = inner.attributes
    override def xmppStanzaError: XmppStanzaError = inner.xmppStanzaError
  }
}
