package com.github.rgafiyatullin.xmpp_protocol.stanzas.features

import com.github.rgafiyatullin.xml.common.QName
import com.github.rgafiyatullin.xml.dom.Node
import com.github.rgafiyatullin.xmpp_protocol.XmppConstants
import com.github.rgafiyatullin.xmpp_protocol.stanzas.Stanza

import scala.util.Try

trait Features
  extends Stanza
{
  override def toXml: Node =
    Node(XmppConstants.names.streams.features)
      .withChildren(features.map(_.toXml))

  def features: Seq[Feature]

  def withFeatures(other: Seq[Feature]): Features =
    new Features.Wrapper(this) { override def features: Seq[Feature] = other }


  def get(qn: QName): Option[Feature] =
    features.find(_.qName == qn)

  def mapFeature(qn: QName)(f: Feature => Feature): Features =
    withFeatures(features.map {
      case feature if feature.qName == qn => f(feature)
      case feature if feature.qName != qn => feature
    })

  def appendFeature(anotherFeature: Feature): Features =
    withFeatures(features :+ anotherFeature)

  def :+(anotherFeature: Feature): Features =
    appendFeature(anotherFeature)
}

object Features {
  def apply(): Features = Root

  def decode: Stanza.Decoder[Features] =
    new Stanza.Decoder[Features] {
      override def isDefinedAt(xml: Node): Boolean =
        xml.qName == XmppConstants.names.streams.features

      override def apply(xml: Node): Try[Features] = {
        assert(xml.qName == XmppConstants.names.streams.features)

        Try( Features().withFeatures(xml.children.map(Feature.fromXml)) )
      }
    }

  private case object Root extends Features {
    override def features: Seq[Feature] = Seq.empty
  }

  private class Wrapper(inner: Features) extends Features {
    override def features: Seq[Feature] = inner.features
  }
}

