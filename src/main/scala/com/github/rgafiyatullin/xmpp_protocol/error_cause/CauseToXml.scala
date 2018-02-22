package com.github.rgafiyatullin.xmpp_protocol.error_cause

import com.github.rgafiyatullin.xml.dom.{CData, Node}
import com.github.rgafiyatullin.xmpp_protocol.XmppConstants

trait CauseToXml extends Exception {
  def causesToXml(dumpCauses: Boolean): Option[Node] =
    if (!dumpCauses) None
    else causeOptionToXmlOption(Option(getCause))

  def causeToXml(cause: Throwable): Node =
    Node(XmppConstants.names.cause.cause)
      .withChildren(
        Seq(
          Node(XmppConstants.names.cause.message)
            .withChildren(Seq(CData(cause.getMessage))),
          Node(XmppConstants.names.cause.stack)
            .withChildren(
              cause.getStackTrace.map(ste =>
                Node(XmppConstants.names.cause.frame)
                  .withChildren(Seq(CData(ste.toString)))))
        ) ++ causeOptionToXmlOption(Option(cause.getCause)).toSeq)

  def causeOptionToXmlOption(causeOption: Option[Throwable]): Option[Node] =
    for { cause <- causeOption } yield causeToXml(cause)
}
