import functional.part3.applicative._
import functional.part3.functor._
import functional.part5.parserCombinatorApplicativeWithOr._
import functional.part5.parserCombinatorApplicativeWithOr.{Parser => P}

// Now with applicative we can combine multiple parser one after another

val walkParser = P.str("walk") ** (P.str("ing") | P.str("ed") | P.str("s")).optional()

walkParser.run(Location("walk"))
walkParser.run(Location("walking"))
walkParser.run(Location("walked"))
walkParser.run(Location("walks"))
walkParser.run(Location("walkter"))


// Simple computation parser
val intParser = P.regex("-?\\d+".r).map(_.toInt)

// here is a domain sopecific combinator we can build
def op(symbol: Char, f: (x: Int, y: Int) => Int): Parser[(Int, Int) => Int] =
  (P.str(symbol.toString) ** Applicative[Parser].pure(f)).map(_._2)

// this will be easy to update if we want new operators
val operator: Parser[(Int, Int) => Int] =
  op('*', _ * _)
    | op('+', _ + _)
    | op('-', _ - _)
    | op('/', _ / _)
    | op('^', math.pow(_,_).toInt)


val computationParser =
  Applicative[Parser].map3(
    intParser.desc("first argument"),
    operator.desc("operator"),
    intParser.desc("second argument"),
    (arg1, op, arg2) => op(arg1, arg2)
  )

computationParser.run(Location("72+12"))
computationParser.run(Location("64/8"))
computationParser.run(Location("3*3"))
computationParser.run(Location("3**3"))
computationParser.run(Location("3^2"))

// sequence
val emailParser = List(
  P.regex("\\w+".r),
  P.str("@"),
  P.regex("\\w+".r),
  P.str("."),
  P.str("com") | P.str("net")
).sequence


emailParser.run(Location("jimmy@gmail.com"))
emailParser.run(Location("larry@outlook.net"))
emailParser.run(Location("larry@outlook"))

