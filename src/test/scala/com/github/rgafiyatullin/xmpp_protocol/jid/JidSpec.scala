package com.github.rgafiyatullin.xmpp_protocol.jid

import org.scalatest.{FlatSpec, Matchers}

class JidSpec extends FlatSpec with Matchers {
  "A JID" should "implement Equals[Jid] trait properly" in {
    val (a,b,c,a_,b_,c_) = ("a", "b", "c", "D", "E", "F")
    val abc = Jid(a, b, c)
    val abc_ = Jid(a_,b_,c_)

    Jid(a, b, c) == abc should be (true)
    Jid(a, b_,c) == abc should be (false)
    Jid(a_,b, c) == abc should be (false)
    Jid(a, b,c_) == abc should be (false)

    abc canEqual abc_ should be (true)
    abc canEqual Jid should be (false)
  }

  it should "normalize in equality check" in {
    Jid.create("hOsT") == Jid.create("HoSt") should be (true)
    Jid.create("nOdE", "HoSt") == Jid.create("NoDe", "hOsT") should be (true)
  }

  it should "not change res-part upon normalization" in {
    Jid.create("nOdE", "HoSt", "rEs") == Jid.create("NoDe", "hOsT", "rEs") should be (true)
    Jid.create("nOdE", "HoSt", "ReS") == Jid.create("NoDe", "hOsT", "rEs") should be (false)
  }

  it should "normalize when toString is called" in {
    Jid.create("hOsT").toString == Jid.create("HoSt").toString should be (true)
    Jid.create("nOdE", "HoSt").toString == Jid.create("NoDe", "hOsT").toString should be (true)
    Jid.create("nOdE", "HoSt", "rEs").toString == Jid.create("NoDe", "hOsT", "rEs").toString should be (true)
    Jid.create("nOdE", "HoSt", "ReS").toString == Jid.create("NoDe", "hOsT", "rEs").toString should be (false)
    Jid("nOdE", "hOsT", "rEs").toString == "node@host/rEs" should be (true)
    Jid("nOdE", "hOsT", "rEs").toString == "node@host/res" should be (false)
  }

  it should "be parsed" in {
    Jid.parse("h") == Jid.create("h") should be (true)
    Jid.parse("h/r") == Jid.create(Jid.nil, "h", "r") should be (true)
    Jid.parse("n@h") == Jid.create("n", "h") should be (true)
    Jid.parse("n@h/") == Jid.create("n", "h") should be (true)
    Jid.parse("n@h/r") == Jid.create("n", "h", "r") should be (true)

    Jid.parse("h/a@b.c/d") == Jid.create("h").withResource("a@b.c/d") should be (true)
    Jid.parse("n@h/a@b.c/d") == Jid.create("n", "h", "a@b.c/d") should be (true)
  }

  it should "change parts" in {
    val j = Jid.parse("n@h/r")
    val jh1 = Jid.parse("n@h1/r")
    val jr1 = Jid.parse("n@h/r1")
    val jn1 = Jid.parse("n1@h/r")
    j.withHost("h1") == jh1 should be (true)
    j.withNode("n1") == jn1 should be (true)
    j.withResource("r1") == jr1 should be (true)
  }

  it should "be converted to bare" in {
    val jf = Jid.parse("n@h/r")
    val jb = Jid.parse("n@h")

    val jh = Jid.parse("h/r")
    val jhb = Jid.parse("h")

    jf.toBare == jb should be (true)
    jh.toBare == jhb should be (true)
  }

  it should "be converted to host" in {
    val jf = Jid.parse("n@h/r")
    val jfh = Jid.parse("h/r")

    val jb = Jid.parse("a@h")
    val jbh = Jid.parse("h")

    jf.toHost == jfh should be (true)
    jb.toHost == jbh should be (true)
  }
}