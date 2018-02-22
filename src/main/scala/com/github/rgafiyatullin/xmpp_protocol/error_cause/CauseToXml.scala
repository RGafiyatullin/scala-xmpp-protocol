package com.github.rgafiyatullin.xmpp_protocol.error_cause

import com.github.rgafiyatullin.xml.common.QName
import com.github.rgafiyatullin.xml.dom.{CData, Node}
import com.github.rgafiyatullin.xmpp_protocol.XmppConstants

trait CauseToXml extends Exception {
  def causesToXml(dumpCauses: Boolean): Option[Node] =
    if (!dumpCauses) None
    else causeOptionToXmlOption(Option(getCause))

  private def textNode(qn: QName, text: String): Node =
    Node(qn).withChildren(Seq(CData(text)))

  def causeToXml(cause: Throwable): Node =
    Node(XmppConstants.names.cause.cause)
      .withChildren(
        Seq(
          textNode(XmppConstants.names.cause.exceptionType, cause.getClass.getCanonicalName),
          textNode(XmppConstants.names.cause.message, cause.getMessage),
          Node(XmppConstants.names.cause.stack)
            .withChildren(
              cause.getStackTrace.map(ste =>
                textNode(XmppConstants.names.cause.frame, ste.toString)))
        ) ++
          causeOptionToXmlOption(Option(cause.getCause)).toSeq)

  def causeOptionToXmlOption(causeOption: Option[Throwable]): Option[Node] =
    for { cause <- causeOption } yield causeToXml(cause)
}
