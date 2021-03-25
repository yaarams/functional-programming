import functional.part3.functor._
import functional.part3.applicative._
import functional.part5.parserCombinatorApplicative._
import functional.part5.parserCombinatorApplicative.{Parser => P} // to get the functor extension methods

// Now with applicative we can combine multiple parser one after another


val abParser = P.str("a") ** P.str("b")

abParser.run(Location("b"))
abParser.run(Location("a"))
abParser.run(Location("ab"))
abParser.run(Location("abc"))


// Simple computation parser

val intParser = P.regex("-?\\d+".r).map(_.toInt)

val computationParser =
  Applicative[Parser].map3(
    intParser.desc("first argument"),
    P.regex("[+\\-/*]".r).desc("operator"),
    intParser.desc("second argument"),
    (arg1, op, arg2) =>
      if(op == "+") {
        arg1 + arg2
      } else if(op == "-") {
        arg1 - arg2
      } else if(op == "*") {
        arg1 * arg2
      } else {
        arg1 / arg2
      }
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
  P.regex("com|net".r)
).sequence


emailParser.run(Location("jimmy@gmail.com"))
emailParser.run(Location("larry@outlook.net"))
emailParser.run(Location("larry@outlook"))

