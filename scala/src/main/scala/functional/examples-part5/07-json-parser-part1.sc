
import functional.part3.applicative._
import functional.part3.functor._
import functional.part5.parserCombinatorApplicativeWithBetterQuanitfiers._
import functional.part5.parserCombinatorApplicativeWithBetterQuanitfiers.Parser._


// In this example we will ignore whitespaces for the most part
// it't not hard to add optional whitespaces everywhere we need
// whitespace = P.regex("\\s")
// whitespace.zeroOrMoreTimes() ...

trait Json

case class JsonObject(entries: Map[String, Json]) extends Json
case class JsonArray(values: List[Json]) extends Json
case class JsonNumber(value: Double) extends Json
case class JsonString(value: String) extends Json
case class JsonBool(value: Boolean) extends Json


val booleanValue = (str("true") | str("false")).map(v => JsonBool(v == "true"))
val numericValue = regex("-?\\d+(\\.\\d+)?".r).map(v => JsonNumber(v.toDouble))
val stringValue = (str("\"") >> regex("[^\"]+".r) << str("\"")).map(v => JsonString(v))

val primitiveValue = booleanValue | numericValue | stringValue

primitiveValue.run(Location("true"))
primitiveValue.run(Location("-13.7"))
primitiveValue.run(Location("\"abc\""))


val array = str("[") >> primitiveValue.zeroOrMoreTimes(delimiter=",") << str("]")

array.run(Location("[]"))
array.run(Location("[true]"))
array.run(Location("[true,12.6,\"word\"]"))
array.run(Location("[true, 12.6, \"word\"]"))

val whitespace = regex("\\s".r)
val commaWithSpaces = whitespace.zeroOrMoreTimes() >> str(",") << whitespace.zeroOrMoreTimes()
val arrayWithSpaces = str("[") >> primitiveValue.zeroOrMoreTimes(delimiterParser=commaWithSpaces) << str("]")


arrayWithSpaces.run(Location("[true, 12.6, \"word\"]"))


// with object we can do the same optional whitespace tratment but will ignore to keep the example shorter

val objectKey = (str("\"") >> regex("[^\"]+".r) << str("\""))
val objectEntry = objectKey ** (str(":") >> primitiveValue)

objectEntry.run(Location("\"a\":7"))
objectEntry.run(Location("\"a\":true"))


val objectP = (str("{") >> objectEntry.zeroOrMoreTimes(",") << str("}")).map(entries => entries.to(Map))

objectP.run(Location("{\"name\":\"john\",\"age\":29}"))

// But how do we define an array of arrays? we have a recursive structure here
// We can't just say something like array = (primitveValue | array).zeroOrMoreTimes(",") ???