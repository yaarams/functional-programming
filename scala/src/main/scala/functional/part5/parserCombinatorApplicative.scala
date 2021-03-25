package functional.part5

import functional.part3.functor._
import functional.part3.applicative._
import functional.part3.monad._

import scala.util.matching.Regex
import scala.util.matching.Regex.Match

object parserCombinatorApplicative {

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

  // NOTE: We can define a Monad for result (like Either) but we wont be using so we will do with Functor 
  given Functor[Result] with {
    def map[A, B](fa: Result[A], f: A => B): Result[B] = fa match {
      case err@Failure(_) => err
      case Success(v, offset) => Success(f(v), offset)
    }
  }

  // parser 
  case class Parser[+A](run: Location => Result[A]) {

    // Add ability to change the errors, not just the results (similar to eithers mapLeft)
    def mapError(f: ParserError => ParserError): Parser[A] = Parser {
      loc =>
        run(loc) match {
          case succ@Success(_,_) => succ
          case Failure(err) => Failure(f(err))
        }
    }

    def desc(name: String): Parser[A] = this.mapError(err =>
      ParserError(err.loc, f"Expected '$name' at ${err.loc.offset}" +
        f" but found: ${err.loc.input.slice(err.loc.offset, err.loc.offset + 10)}")
    )
  }


  // functor applicative for parser
  given Applicative[Parser] with {

    def pure[A](a: A): Parser[A] = Parser {
      _ => Success(a, 0) // pure values don't consume anything from the input
    }

    override def map2[A, B, C](pa: Parser[A], pb: Parser[B], f: (A, B) => C): Parser[C] = Parser {
      loc0 =>
        pa.run(loc0) match {
          case err@Failure(_) => err
          case Success(v1, consumed1) =>
            val loc1 = Location(loc0.input, loc0.offset + consumed1)
            pb.run(loc1) match {
              case err@Failure(_) => err
              case Success(v2, consumed2) =>
                Success(f(v1, v2), consumed1 + consumed2)
            }
        }
    }
  }

  // Product will be very useful so lets create an operator for it
  extension [A, B](pa: Parser[A]) {
   
    def **(pb: Parser[B]): Parser[(A, B)] = Applicative[Parser].product(pa, pb)
    
  }
  
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
