import functional.part3.applicative._
import functional.part3.functor._
import functional.part5.parserCombinatorApplicativeWithBadQuanitfiers._
import functional.part5.parserCombinatorApplicativeWithBadQuanitfiers.{Parser => P}

// Now with applicative we can combine multiple parser one after another

val threeAs = P.str("a").repeat(3)

threeAs.run(Location("a"))
threeAs.run(Location("aa"))
threeAs.run(Location("aaa"))
threeAs.run(Location("aaaa"))

val atMostTwoAs = P.str("a").atMost(2)

atMostTwoAs.run(Location(""))
atMostTwoAs.run(Location("a"))
atMostTwoAs.run(Location("aa"))
atMostTwoAs.run(Location("aaa"))
atMostTwoAs.run(Location("aaaa"))

val betweenTwoAndFour = P.str("a").times(2, 4)

betweenTwoAndFour.run(Location(""))
betweenTwoAndFour.run(Location("a"))
betweenTwoAndFour.run(Location("aa"))
betweenTwoAndFour.run(Location("aaa"))
betweenTwoAndFour.run(Location("aaaa"))
betweenTwoAndFour.run(Location("aaaaa"))


// If we try to use this, we will have a StackOverflow exception

//val oneOrMoreAs = P.str("a").oneOrMoreTimes()
//
//betweenTwoAndFour.run(Location(""))
//betweenTwoAndFour.run(Location("a"))
//betweenTwoAndFour.run(Location("aa"))
//betweenTwoAndFour.run(Location("aaa"))
//betweenTwoAndFour.run(Location("aaaa"))
//betweenTwoAndFour.run(Location("aaaaa"))

