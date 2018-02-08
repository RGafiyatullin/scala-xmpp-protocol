package com.github.rgafiyatullin.xmpp_protocol.stanzas

import com.github.rgafiyatullin.xml.common.Attribute
import com.github.rgafiyatullin.xml.dom.Node
import com.github.rgafiyatullin.xmpp_protocol.stanza_error.XmppStanzaError

import scala.util.{Success, Try}

trait Stanza {
  def toXml: Node
}

object Stanza {
  final case class Unsupported(node: Node)
    extends Stanza
      with HasIDOption[Unsupported]
      with HasAttributes[Unsupported]
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
    }
  }
  trait HasAttributes[+Self <: HasAttributes[Self]] extends HasAttributes.Untyped {
    override def withAttribute(name: String, value: String): Self
    override def withoutAttribute(name: String): Self
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
    sealed trait Untyped {
      def body: Node
      def withBody(newBody: Node): Untyped
    }
  }
  trait HasBody[+Self <: HasBody[Self]] extends HasBody.Untyped {
    override def body: Node
    override def withBody(newBody: Node): Self
  }

  object HasBodyOption {
    sealed trait Untyped {
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
