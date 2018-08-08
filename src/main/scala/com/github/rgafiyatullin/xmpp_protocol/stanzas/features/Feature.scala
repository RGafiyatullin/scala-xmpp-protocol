package com.github.rgafiyatullin.xmpp_protocol.stanzas.features

import com.github.rgafiyatullin.xml.common.QName
import com.github.rgafiyatullin.xml.dom.Node
import com.github.rgafiyatullin.xmpp_protocol.XmppConstants

trait Feature {
  def qName: QName
  def withQName(other: QName): Feature =
    new Feature.Wrapper(this) { override def qName: QName = other }

  def required: Boolean
  def withRequired(other: Boolean): Feature =
    new Feature.Wrapper(this) { override def required: Boolean = other }


  def children: Seq[Node]
  def withChildren(other: Seq[Node]): Feature =
    new Feature.Wrapper(this) { override def children: Seq[Node] = other }

  def toXml: Node =
    Node(qName)
      .withChildren(
        Some(requiredXml).filter(_ => required).toSeq ++ children)

  private val requiredXml: Node =
    Node(Feature.qnRequired(qName.ns))
}

object Feature {
  def apply(qn: QName): Feature = Root(qn)

  def fromXml(xml: Node): Feature =
    Feature(xml.qName)
      .withRequired( xml.children.exists(_.qName == qnRequired(xml.qName.ns)) )
      .withChildren( xml.children.filter(_.qName != qnRequired(xml.qName.ns)) )

  private final case class Root(qName: QName) extends Feature {
    override def required: Boolean = false
    override def children: Seq[Node] = Seq.empty
  }

  private class Wrapper(inner: Feature) extends Feature {
    override def qName: QName = inner.qName
    override def required: Boolean = inner.required
    override def children: Seq[Node] = inner.children
  }

  private def qnRequired(ns: String): QName =
    QName(ns, XmppConstants.names.streams.required.localName)
}
