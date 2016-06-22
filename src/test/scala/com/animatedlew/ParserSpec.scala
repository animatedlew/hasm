package com.animatedlew

import org.scalatest._

class ParserSpec extends FlatSpec with Matchers {
  val p = new HackParser
  "Hasm" should "parse C-instructions" in {
    // "AD=D+M => type = 111, a = 1, op = 000010, d = 110, j = 000"
    p.parseAll(p.cinstruction, "AD=D+M").get shouldBe "1111000010110000"
    p.parseAll(p.cinstruction, "0;JMP").get  shouldBe "1110101010000111"
    p.parseAll(p.cinstruction, "AD=D+M").get shouldBe "1111000010110000"
    p.parseAll(p.cinstruction, "D=M-D").get  shouldBe "1111000111010000"
  }
  it should "parse comments" in {
    p.parseAll(p.instruction, "0; JMP // ok").get shouldBe "1110101010000111"
    p.parseAll(p.instruction, "AD=D+M // nothing goes here").get shouldBe "1111000010110000"
    p.parseAll(p.instruction, "D=M-D // this is just a distraction").get shouldBe "1111000111010000"
  }
  it should "parse general instructions" in {
    p.parseAll(p.instruction, "").get shouldBe ""
    p.parseAll(p.instruction, "// ok").get shouldBe ""
    p.parseAll(p.instruction, "(LABEL) // ok").get shouldBe "#LABEL"
    p.parseAll(p.instruction, "@7 // ok").get shouldBe "0000000000000111"
  }
  it should "fail on bad input" in {
    an [RuntimeException] should be thrownBy p.parseAll(p.instruction, "BAD").get
  }
  it should "recognize jumps" in {
    p.parseAll(p.jmp, "; JGT").get should be (Some("001"))
  }
}
