package functional.part5

import functional.part3.monad.Monad

import scala.util.matching.Regex
import scala.util.matching.Regex.Match

object monadicParserCombinator {

  case class ParserError(message: String, offset: Int)
  
  case class ParserState(input: String, offset: Int) {
    
   def withoutPrefixOf(size: Int) = 
     ParserState(input.substring(size), offset + size)
  }

  case class Parser[A](
    parsePartial: (ParserState) => Either[ParserError, (ParserState, A)] 
  )
 
  given Monad[Parser] {

    override def pure[A](a: A): Parser[A] = Parser((state) => Right((state, a)))
    
    override def flatMap[A, B](ma: Parser[A], f: A => Parser[B]): Parser[B] = Parser {
      state =>
        ma.parsePartial(state) match {
          // by rewrapping the error we get Either[Err, A] into Either[Err, B]
          // this could have also been done with casting
          case Left(err) => Left(err)
          case Right((state1, a)) => 
            f(a).parsePartial(state1)
        }
    }
  } 
  
  object Parser {

    def str(expected: String): Parser[String] = Parser {
      state =>
        if (state.input.startsWith(expected)) {
          Right(
            state.withoutPrefixOf(expected.length),
            expected
          )
        } else {
          Left(ParserError(
            f"Expected '$expected' at ${state.offset} but got '${state.input.substring(expected.length)}'",
            state.offset)
          )
        }
    }

    def regex(expectedRegex: Regex): Parser[String] = Parser {
      state =>
        expectedRegex.findPrefixOf(state.input) match {
          case Some(m) =>
            Right((
              state.withoutPrefixOf(m.length),
              m.toString
            ))
          case None =>
            Left(ParserError(
              f"Expected '/${expectedRegex.pattern}/' at ${state.offset} but got '${state.input.substring(10)}...'",
              state.offset)
            )
        }
    }
  } 
}
