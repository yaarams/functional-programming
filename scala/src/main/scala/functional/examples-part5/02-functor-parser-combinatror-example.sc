import functional.part5.parserCombinatorFunctor._
import functional.part5.parserCombinatorFunctor.{Parser => P}

import functional.part3.functor.* // to get the functor extension methods

// very basic use of our exiting parsers
P.str("aba").run(Location("aaa"))
P.str("aba").run(Location("aba"))

P.regex("aba+".r).run(Location("aaa"))
P.regex("aba+".r).run(Location("ab"))
P.regex("aba+".r).run(Location("abaaa"))
P.regex("aba+".r).run(Location("abaaa0123"))

// with map, these new parsers become trivial

val intParser = P.regex("-?\\d+".r).map(_.toInt).desc("integer number")

intParser.run(Location("123"))
intParser.run(Location("-19abc"))
intParser.run(Location("abc"))

val doubleParser = P.regex("-?\\d+(\\.\\d+)?".r).map(_.toDouble).desc("double number")

doubleParser.run(Location("123"))
doubleParser.run(Location("-19.25abc"))
doubleParser.run(Location("abc"))


// Or even very complex
def delmitedBy(value: String, delimiter: String): Parser[List[String]] = Parser {
  loc =>
    var found = 0
    var offset = 0
    var subslice = loc.input.substring(loc.offset)

    if(subslice.startsWith(value)) {
      found += 1
      offset += value.length
      subslice = subslice.substring(value.length)
    }

    val element = delimiter + value

    while (subslice.startsWith(element)) {
      found += 1
      offset += element.length
      subslice = subslice.substring(element.length)
    }

    Success(List.fill(found)(value), loc.advance(offset))
}


delmitedBy("a", ",").run(Location("a,a,a,a,b"))
delmitedBy("ab", ",").run(Location("ab,ab,a,a,b"))


// With map this also become simple
def countDelmitedBy(value: String, delimiter: String): Parser[Int] =
  delmitedBy(value, delimiter).map(_.length)


countDelmitedBy("a", ",").run(Location("a,a,a,a,b"))
countDelmitedBy("ab", ",").run(Location("ab,ab,a,a,b"))
