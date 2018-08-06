package com.github.rgafiyatullin.xmpp_protocol.stanzas

import com.github.rgafiyatullin.xml.common.{Attribute, QName}
import com.github.rgafiyatullin.xml.dom.Node
import com.github.rgafiyatullin.xmpp_protocol.jid.Jid
import com.github.rgafiyatullin.xmpp_protocol.stanza_error.XmppStanzaError

import scala.util.{Success, Try}

trait Stanza {
  def toXml: Node

  override def toString: String =
    s"Stanza[xml: ${toXml.rendered}]"
}

object Stanza {
  final case class Unsupported(node: Node)
    extends Stanza
      with HasIDOption[Unsupported]
      with HasAttributes[Unsupported]
      with HasChildren[Unsupported]
  {
    override def idOption: Option[String] = node.attribute("id")
    override def withIdOption(newIdOption: Option[String]): Unsupported =
      copy(node = node.withAttribute("id", newIdOption))

    override def attributes: Map[String, String] =
      node.attributes
        .collect { case Attribute.Unprefixed(name, value) => name -> value }
        .toMap

    override def withAttribute(name: String, value: String): Unsupported =
      copy(node = node.withAttribute(name, value))

    override def withoutAttribute(name: String): Unsupported =
      copy(node = node.withAttribute(name, None))


    override def children: Seq[Node] = node.children

    override def withChildren(newChildren: Seq[Node]): Unsupported =
      copy(node = node.withChildren(newChildren))

    override def toXml: Node = node
  }

  object Unsupported {
    def decode: Decoder[Unsupported] =
      new Decoder[Unsupported] {
        override def isDefinedAt(node: Node): Boolean = true
        override def apply(node: Node): Try[Unsupported] =
          Success(Unsupported(node))
      }
  }

  object HasID {
    sealed trait Untyped extends Stanza {
      def id: String
      def withId(newId: String): Untyped
    }
  }
  trait HasID[+Self <: HasID[Self]] extends HasID.Untyped {
    override def id: String
    override def withId(newId: String): Self
  }

  object HasIDOption {
    sealed trait Untyped extends Stanza {
      def idOption: Option[String]
      def withIdOption(newIdOption: Option[String]): Untyped
      def withId(id: String): Untyped
      def withoutId: Untyped
    }
  }
  trait HasIDOption[+Self <: HasIDOption[Self]] extends HasIDOption.Untyped {
    override def idOption: Option[String]
    override def withIdOption(newIdOption: Option[String]): Self

    final override def withId(id: String): Self =
      withIdOption(Some(id))

    final override def withoutId: Self =
      withIdOption(None)
  }

  object HasAttributes {
    sealed trait Untyped extends Stanza {
      def attributes: Map[String, String]
      def withAttribute(name: String, value: String): Untyped
      def withoutAttribute(name: String): Untyped

      final def attributeOption(name: String): Option[String] =
        attributes.get(name)
    }
  }
  trait HasAttributes[+Self <: HasAttributes[Self]] extends HasAttributes.Untyped {
    override def withAttribute(name: String, value: String): Self
    override def withoutAttribute(name: String): Self

    final def withAttributeOption(name: String, valueOption: Option[String]): Self =
      valueOption.fold(withoutAttribute(name))(withAttribute(name, _))
  }

  object HasAddresses {
    sealed trait Untyped extends Stanza {
      this: Stanza.HasAttributes.Untyped =>
      def fromOption: Option[Jid] =
        attributeOption("from").map(Jid.parse)
      def from: Jid =
        fromOption.get

      def toOption: Option[Jid] =
        attributeOption("to").map(Jid.parse)
      def to: Jid =
        toOption.get
    }
  }
  trait HasAddresses[+Self <: HasAddresses[Self] with HasAttributes[Self]]
    extends HasAddresses.Untyped with HasAttributes[Self]
  {
    def withFrom(jid: Jid): Self = withAttribute("from", jid.toString)
    def withoutFrom: Self = withoutAttribute("from")
    final def withFromOption(jidOption: Option[Jid]): Self =
      jidOption.fold(withoutFrom)(withFrom)

    def withTo(jid: Jid): Self = withAttribute("to", jid.toString)
    def withoutTo: Self = withoutAttribute("to")
    final def withToOption(jidOption: Option[Jid]): Self =
      jidOption.fold(withoutTo)(withTo)
  }

  object HasChildren {
    sealed trait Untyped extends Stanza {
      def children: Seq[Node]
      def withChildren(newChildren: Seq[Node]): Untyped
    }
  }
  trait HasChildren[+Self <: HasChildren[Self]] extends HasChildren.Untyped {
    override def children: Seq[Node]
    override def withChildren(newChildren: Seq[Node]): Self
  }

  trait HasError[+Error <: Stanza] extends HasError.Untyped {
    override def error(xmppStanzaError: XmppStanzaError): Error
  }
  object HasError {
    trait Untyped extends Stanza {
      def error(xmppStanzaError: XmppStanzaError): Stanza
    }
  }

  object HasBody {
    sealed trait Untyped extends Stanza {
      def body: Node
      def withBody(newBody: Node): Untyped
    }
  }
  trait HasBody[+Self <: HasBody[Self]] extends HasBody.Untyped {
    override def body: Node
    override def withBody(newBody: Node): Self
  }

  object HasBodyOption {
    sealed trait Untyped extends Stanza {
      def bodyOption: Option[Node]
      def withBodyOption(newBodyOption: Option[Node]): Untyped
      def withBody(newBody: Node): Untyped
      def withoutBody: Untyped
    }
  }
  trait HasBodyOption[+Self <: HasBodyOption[Self]] extends HasBodyOption.Untyped {
    override def bodyOption: Option[Node]
    override def withBodyOption(newBodyOption: Option[Node]): Self

    final override def withBody(newBody: Node): Self =
      withBodyOption(Some(newBody))
    final override def withoutBody: Self =
      withBodyOption(None)
  }

  
  type Decoder[+S <: Stanza] = PartialFunction[Node, Try[S]]
}
