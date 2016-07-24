package com.animatedlew

import java.io.FileReader
import scala.language.{ postfixOps, implicitConversions }
import scala.util.parsing.combinator._
import scala.collection.mutable

class HackParser extends JavaTokenParsers {

  protected override val whiteSpace = """(?:[ \t]|(/\*(\*(?!/)|[^*])*\*/)|//.*)+""".r
  private val eol = """\r?\n""".r
  val RegisterRegex = "R([0-9]{1,2})".r
  val LabelRegex = """^[a-zA-Z$_.][a-zA-Z0-9$_.]+""".r
  var variableAddress = 0x10
  val LabelMarker = '#'
  var lineCount = 0
  val epsilon = ""

  implicit def stringToSymbol(s: String): Symbol = Symbol(s)

  var symbols = mutable.Map(
    'R0       -> pad16bit(0),
    'R1       -> pad16bit(1),
    'R2       -> pad16bit(2),
    'R3       -> pad16bit(3),
    'R4       -> pad16bit(4),
    'R5       -> pad16bit(5),
    'R6       -> pad16bit(6),
    'R7       -> pad16bit(7),
    'R8       -> pad16bit(8),
    'R9       -> pad16bit(9),
    'R10      -> pad16bit(10),
    'R11      -> pad16bit(11),
    'R12      -> pad16bit(12),
    'R13      -> pad16bit(13),
    'R14      -> pad16bit(14),
    'R15      -> pad16bit(15),
    'SP       -> pad16bit(0),
    'LCL      -> pad16bit(1),
    'ARG      -> pad16bit(2),
    'THIS     -> pad16bit(3),
    'THAT     -> pad16bit(4),
    'SCREEN   -> "0100000000000000",
    'KBD      -> "0110000000000000"
  ) withDefault { _.name }

  val plus = "+"
  val minus = "-"
  val and = "&"
  val or = "|"
  val eq = "="
  val D = """(?i)D""".r
  val negD = """(?i)-D""".r
  val notD = """(?i)!D""".r
  val A = """(?i)A""".r
  val negA = """(?i)-A""".r
  val notA = """(?i)!A""".r
  val M = """(?i)M""".r
  val negM = """(?i)-M""".r
  val notM = """(?i)!M""".r
  val negOne = "-1"
  val one = "1"
  val zero = "0"

  val dest = ((
      A ~ M ~ D       ^^ { _ => "111" }
    | A ~ M           ^^ { _ => "101" }
    | A ~ D           ^^ { _ => "110" }
    | M ~ D           ^^ { _ => "011" }
    | A               ^^ { _ => "100" }
    | M               ^^ { _ => "001" }
    | D               ^^ { _ => "010" }
    ) <~ eq)?

  val comp = (
      A ~ plus ~ one  ^^ { _ => "0110111" }
    | A ~ minus ~ D   ^^ { _ => "0000111" }
    | A ~ minus ~ one ^^ { _ => "0110010" }
    | notA            ^^ { _ => "0110001" }
    | negA            ^^ { _ => "0110011" }
    | A               ^^ { _ => "0110000" }

    | D ~ plus ~ one  ^^ { _ => "0011111" }
    | D ~ minus ~ one ^^ { _ => "0001110" }

    | D ~ plus ~ A    ^^ { _ => "0000010" }
    | A ~ plus ~ D    ^^ { _ => "0000010" }
    | D ~ minus ~ A   ^^ { _ => "0010011" }
    | D ~ and ~ A     ^^ { _ => "0000000" } // also same as mod
    | D ~ or ~ A      ^^ { _ => "0010101" }

    | D ~ plus ~ M    ^^ { _ => "1000010" }
    | M ~ plus ~ D    ^^ { _ => "1000010" }
    | D ~ minus ~ M   ^^ { _ => "1010011" }
    | D ~ and ~ M     ^^ { _ => "1000000" }
    | D ~ or ~ M      ^^ { _ => "1010101" }

    | notD            ^^ { _ => "0001101" }
    | negD            ^^ { _ => "0001111" }
    | D               ^^ { _ => "0001100" }

    | M ~ plus ~ one  ^^ { _ => "1110111" }
    | M ~ minus ~ one ^^ { _ => "1110010" }
    | M ~ minus ~ D   ^^ { _ => "1000111" }
    | notM            ^^ { _ => "1110001" }
    | negM            ^^ { _ => "1110011" }
    | M               ^^ { _ => "1110000" }

    | zero            ^^ { _ => "0101010" }
    | one             ^^ { _ => "0111111" }
    | negOne          ^^ { _ => "0111010" }
  )

  val jmp = (";" ~> (
      "JGT" ^^ { _ => "001" }
    | "JEQ" ^^ { _ => "010" }
    | "JGE" ^^ { _ => "011" }
    | "JLT" ^^ { _ => "100" }
    | "JNE" ^^ { _ => "101" }
    | "JLE" ^^ { _ => "110" }
    | "JMP" ^^ { _ => "111" }
  ))?

  val cinstruction = dest ~ comp ~ jmp ^^ {
    case d ~ c ~ j => s"111$c${d.getOrElse("000")}${j.getOrElse("000")}"
  }

  val label = LabelRegex ^^ { symbols(_) }

  val ainstruction = "@" ~> (wholeNumber ^^ { n =>
    val str = n.toLong.toBinaryString
    "0" * (16 - str.length) + str
  } | label)

  val linstruction = "(" ~> label <~ ")" ^^ {
    case l@RegisterRegex(n) if (0 to 15).contains(n) => symbols(l)
    case "KBD"    => symbols('KBD)
    case "SCREEN" => symbols('SCREEN)
    case "SP"     => symbols('SP)
    case "LCL"    => symbols('LCL)
    case "ARG"    => symbols('ARG)
    case "THIS"   => symbols('THIS)
    case "THAT"   => symbols('THAT)
    case variable => s"$LabelMarker$variable"
  }

  val instruction = (cinstruction | ainstruction | linstruction | epsilon) ^? (
    { case op => if (op.nonEmpty) lineCount += 1; op },
    op => s"Error @ line: $lineCount\nOpcode: $op"
  )

  def multiline = rep(instruction <~ rep1(eol))

  def run(f: FileReader) = parseAll(multiline, f) match {
    case Success(result, _) =>
      val withoutEmpties = result.toVector.filter(_.trim.nonEmpty)
      assignAddresses(withoutEmpties)
    case failure: NoSuccess => scala.sys.error(s">> ${failure.msg}")
  }

  private def assignAddresses(ops: Vector[String]) = {
    var address = 0
    val pairs = ops.map { op =>
      val loc = if (op.head == LabelMarker) {
        symbols(op.tail) = pad16bit(address)
        new String
      } else {
        val c = address
        address += 1
        pad16bit(c)
      }
      loc -> op
    } filterNot {
      case (_, l) if l.head == LabelMarker => true
      case _ => false
    }
    declareVariables(updateAddressRefs(pairs))
  }

  private def updateAddressRefs(ops: Vector[(String, String)]) = {
    ops.map { case (address, op) => address -> symbols(op) }
  }

  private def declareVariables(ops: Vector[(String, String)]) = {
    ops.map { case (address, op) =>
      if (symbols(op) == op && op.matches(LabelRegex.regex)) {
        symbols(op) = pad16bit(variableAddress)
        variableAddress += 1
      }
      address -> symbols(op)
    }
  }

  private def pad16bit(n: Long): String = {
    val s = n.toBinaryString
    f"$s%16s".replace(' ', '0')
  }
}
