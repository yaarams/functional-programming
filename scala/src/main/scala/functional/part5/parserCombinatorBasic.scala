package functional.part5

import scala.util.matching.Regex
import scala.util.matching.Regex.Match

object parserCombinatorBasic {

  // represents a location in the parsed string, input is the full string, offset is where 
  // the current parsing head is
  case class Location(input: String, offset: Int = 0) {

    def getLine(): Int = input.slice(0, offset + 1).count(_ == '\n') + 1

    def getColumn(): Int = input.slice(0, offset + 1).lastIndexOf('\n') match {
      case -1 => offset + 1
      case lastLineStart => offset - lastLineStart
    }
  }

  // location will indicate where the error happened and some message about what went wrong
  case class ParserError(loc: Location, msg: String)

  // This is basically a specialized Either with nicer names and only a single generic param 
  sealed trait Result[+A]
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure[+A](get: ParserError) extends Result[Nothing]

  // parser 
  case class Parser[+A](run: Location => Result[A])

  // --------------------------------- // 
  // Basic concrete parsers definition //
  // --------------------------------- // 

  object Parser {

    def str(expected: String): Parser[String] = Parser {
      loc =>
        if (loc.input.substring(loc.offset).startsWith(expected)) {
          Success(
            expected,
            expected.length
          )
        } else {
          Failure(
            ParserError(
              loc, 
              f"Expected '$expected' at ${loc.offset}"
                + f" but got '${loc.input.slice(loc.offset,loc.offset + expected.length)}'"
            )
          )
        }
    }

    def regex(expectedRegex: Regex): Parser[String] = Parser {
      loc =>
        expectedRegex.findPrefixOf(loc.input.substring(loc.offset)) match {
          case Some(m) =>
            Success(
              m.toString,
              m.length
            )
          case None =>
            Failure(
              ParserError(
                loc,
                f"Expected '/${expectedRegex.pattern}/' at ${loc.offset}"
                  + f" but got '${loc.input.slice(loc.offset, loc.offset + 10)}'"
              )
            )
        }
    }
  }
}
