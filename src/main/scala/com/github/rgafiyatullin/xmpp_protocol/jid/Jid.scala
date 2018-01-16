package com.github.rgafiyatullin.xmpp_protocol.jid

final case class Jid(node: String, host: String, resource: String) extends Equals {
  val isNormalized =
    node == Jid.nodePrep(node) &&
      host == Jid.hostPrep(host) &&
      resource == Jid.resPrep(resource)

  def isBare: Boolean = resource.isEmpty
  def isHost: Boolean = node.isEmpty

  def toBare: Jid = withResource(Jid.nil)
  def toHost: Jid = withNode(Jid.nil)

  def normalize: Jid = Jid.create(node, host, resource)

  def withNode(n: String): Jid = this.copy(node = Jid.nodePrep(n))
  def withHost(h: String): Jid = this.copy(host = Jid.hostPrep(h))
  def withResource(r: String): Jid = this.copy(resource = Jid.resPrep(r))

  override def toString: String =
    if (isNormalized)
      (isHost, isBare) match {
        case (true, true) => host
        case (true, false) => host + "/" + resource
        case (false, true) => node + "@" + host
        case (false, false) => node + "@" + host + "/" + resource
      }
    else
      normalize.toString

  override def hashCode: Int = Jid.unapply(normalize).hashCode()


  override def canEqual(that: Any): Boolean =
    that.isInstanceOf[Jid]

  override def equals(that: Any): Boolean =
    (this canEqual that) && hashCode == that.hashCode

}

object Jid {
  val nil = ""

  def nodePrep(node: String): String = node.toLowerCase
  def hostPrep(host: String): String = host.toLowerCase
  def resPrep(res: String): String = res

  def create(host: String): Jid = create(Jid.nil, host, Jid.nil)

  def create(node: String, host: String, res: String = Jid.nil): Jid =
    Jid(nodePrep(node), hostPrep(host), resPrep(res))

  def parse(s: String): Jid =
    s.indexOf('/') match {
      case -1 => parseBareJid(s)
      case slashIdx =>
        val (bareS, slashRes) = s.splitAt(slashIdx)
        val bareJid = parseBareJid(bareS)
        val resPart = slashRes.drop(1)

        bareJid.withResource(resPart)
    }

  private def parseBareJid(s: String): Jid =
    s.indexOf('@') match {
      case -1 => Jid.create(s)
      case atIndex =>
        val (node, atHost) = s.splitAt(atIndex)
        val host = atHost.drop(1)
        Jid.create(node, host)
    }
}
