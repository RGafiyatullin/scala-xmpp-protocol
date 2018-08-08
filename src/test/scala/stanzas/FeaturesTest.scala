package stanzas

import com.github.rgafiyatullin.xml.common.QName
import com.github.rgafiyatullin.xml.dom.Node
import com.github.rgafiyatullin.xmpp_protocol.XmppConstants
import com.github.rgafiyatullin.xmpp_protocol.stanzas.Stanza
import com.github.rgafiyatullin.xmpp_protocol.stanzas.features.{Feature, Features}

final class FeaturesTest extends StanzasTest {
  import XmppConstants.names.{streams => consts}

  val featuresCount: Int = 100

  val features: IndexedSeq[Feature] =
    for (i <- 1 to featuresCount)
      yield
        Feature(QName(s"ns:$i", "feature"))
          .withRequired(i % 3 == 0)
          .withChildren(
            for (j <- 1 to i % 5)
              yield Node(QName(s"ns:$i", s"child:$j")))

  val featureXmls: IndexedSeq[Node] =
    for (i <- 1 to featuresCount)
      yield
        Node(QName(s"ns:$i", "feature"))
          .withChildren(
            (if (i % 3 == 0)
              Seq[Node](Node(QName(s"ns:$i", "required")))
            else
              Seq.empty)
            ++ (for (j <- 1 to i % 5)
              yield Node(QName(s"ns:$i", s"child:$j"))))

  "each feature" should "render correctly" in
    (features zip featureXmls).foreach { case (f, x) => f.toXml should be (x) }
  it should "parse correctly" in
    (features zip featureXmls).foreach { case (_, x) => Feature.fromXml(x).toXml should be (x) }


  val stanzas: IndexedSeq[Features] =
    for (i <- 0 to featuresCount)
      yield Features().withFeatures(features.take(i))

  val xmls: IndexedSeq[Node] =
    for (i <- 0 to featuresCount)
      yield Node(consts.features).withChildren(featureXmls.take(i))

  val tests: Seq[(Features, Node)] =
      stanzas zip xmls

  println(tests)

  val decoder: Stanza.Decoder[Features] = Features.decode

  "Features stanzas" should "have a sane equality operator" in {
    val stanzas = tests.map(_._1)
    stanzas.foreach { stanza =>
      stanzas.count(_ == stanza) should be (1)
      stanzas.count(_ != stanza) should be (stanzas.size - 1)
      stanza should be (stanza)
    }
  }
  it should "render" in
    tests.foreach { case (stanza, xml) =>
      renderTest(stanza, xml) }

  it should "parse" in
    tests.foreach { case (stanza, xml) =>
      parseTest(decoder)(xml, stanza) }

}
