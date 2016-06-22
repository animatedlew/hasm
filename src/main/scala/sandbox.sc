import scala.util.parsing.combinator.JavaTokenParsers

object sandbox {

  // (3 * 2) + (6 - 7) - 1 * 3
  class Expression extends JavaTokenParsers {
    def expr: Parser[Double] = term ~ rep("+" ~ term | "-" ~ term) ^^ {
      case t ~ rest => rest.foldLeft(t) {
        case (b, "+" ~ a) => b + a
        case (b, "-" ~ a) => b - a
      }
    }

    def term: Parser[Double] = factor ~ rep("*" ~ factor | "/" ~ factor) ^^ {
      case f ~ rest => rest.foldLeft(f) {
        case (b, "*" ~ a) => b * a
        case (b, "/" ~ a) => b / a
      }
    }

    def factor: Parser[Double] = floatingPointNumber ^^ {
      _.toDouble
    } | "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e }
  }

  class JSON extends JavaTokenParsers {

    def obj: Parser[Map[String, Any]] =
      "{" ~> repsep(member, ",") <~ "}" ^^ (Map() ++ _)

    def arr: Parser[List[Any]] =
      "[" ~> repsep(value, ",") <~ "]"

    def member: Parser[(String, Any)] =
      stringLiteral ~ ":" ~ value ^^ { case name ~ ":" ~ value => (name, value) }

    def value: Parser[Any] = (
      obj
        | arr
        | stringLiteral
        | floatingPointNumber ^^ (_.toDouble)
        | "null" ^^ (x => null)
        | "true" ^^ (x => true)
        | "false" ^^ (x => false)
      )

    def run(s: String) = parseAll(value, s)
  }

 val json = """{
    "address book": {
      "name": "John Smith",
      "address": {
        "street": "10 Market Street",
        "city"  : "San Francisco, CA",
        "zip"   : 94111
      },
      "phone numbers": [
        "408 338-4238",
        "408 111-6892"
      ]
    }
  }"""

  val jparser = new JSON
  println(jparser.run(json))
}

