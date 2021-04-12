
import functional.part3.applicative._
import functional.part3.functor._
import functional.part3.monad._
import functional.part5.parserCombinatorMonad.{Parser => P, _}

// We changed the underlying abstraction to monad and everything works the same (as expected)

val threeABs = (P.str("a") | P.str("b")).repeat(3)

threeABs.run(Location("a"))
threeABs.run(Location("aa"))
threeABs.run(Location("aab"))
threeABs.run(Location("baaa"))

val atMostTwoABs = (P.str("a") | P.str("b")).atMost(2)

atMostTwoABs.run(Location(""))
atMostTwoABs.run(Location("a"))
atMostTwoABs.run(Location("ab"))
atMostTwoABs.run(Location("aaa"))
atMostTwoABs.run(Location("baaa"))

val betweenTwoAndFour = (P.str("a") | P.str("b")).times(2, 4)

betweenTwoAndFour.run(Location(""))
betweenTwoAndFour.run(Location("a"))
betweenTwoAndFour.run(Location("ab"))
betweenTwoAndFour.run(Location("aaa"))
betweenTwoAndFour.run(Location("baaa"))
betweenTwoAndFour.run(Location("bbaaa"))


// Now it will work!
val oneOrMoreABs = (P.str("a") | P.str("b")).oneOrMoreTimes()


oneOrMoreABs.run(Location(""))
oneOrMoreABs.run(Location("a"))
oneOrMoreABs.run(Location("ab"))
oneOrMoreABs.run(Location("aaa"))
oneOrMoreABs.run(Location("baaa"))
oneOrMoreABs.run(Location("bbaaa"))
oneOrMoreABs.run(Location("bbaaac"))

val zeroOrMoreABs = (P.str("a") | P.str("b")).zeroOrMoreTimes()

zeroOrMoreABs.run(Location(""))
zeroOrMoreABs.run(Location("c"))
zeroOrMoreABs.run(Location("a"))
zeroOrMoreABs.run(Location("ab"))
zeroOrMoreABs.run(Location("aaa"))
zeroOrMoreABs.run(Location("baaa"))
zeroOrMoreABs.run(Location("bbaaac"))
