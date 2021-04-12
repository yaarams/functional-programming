import functional.part5.parserCombinatorBasic._
import functional.part5.parserCombinatorBasic.{Parser => P}

// very basic use of our exiting parsers
P.str("aba").run(Location("aaa"))
P.str("aba").run(Location("aba"))
P.str("aba").run(Location("abadfe"))

P.regex("aba+".r).run(Location("aaa"))
P.regex("aba+".r).run(Location("ab"))
P.regex("aba+".r).run(Location("abaaa"))
P.regex("aba+".r).run(Location("abaaa0123"))

// we can directly implement some additional parsers

val intParser: Parser[Int] = Parser{
  loc =>
    // lets reuse the regex parser we already defined
    P.regex("-?\\d+".r).run(loc) match {
      case err@Failure(_) => err
      case Success(v, offset) => Success(v.toInt, offset)
    }
}

intParser.run(Location("123"))
intParser.run(Location("-19abc"))
intParser.run(Location("abc"))

val p1 = P.regex("-?\\d+(\\.\\d+)?".r)

val doubleParser: Parser[Double] = Parser{
  loc =>
    // lets reuse the regex parser we already defined
    P.regex("-?\\d+(\\.\\d+)?".r).run(loc) match {
      case err@Failure(_) => err
      case Success(v, offset) => Success(v.toDouble, offset)
    }
}

doubleParser.run(Location("123"))
doubleParser.run(Location("-19.25abc"))
doubleParser.run(Location("abc"))


// Slightly more complex
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


// Very similar but differnt
def countDelmitedBy(value: String, delimiter: String): Parser[Int] = Parser {
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

    Success(found, loc.advance(offset))
}


countDelmitedBy("a", ",").run(Location("a,a,a,a,b"))
countDelmitedBy("ab", ",").run(Location("ab,ab,a,a,b"))
