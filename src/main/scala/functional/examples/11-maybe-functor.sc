import functional.part1.maybeFunctor._

val m1 = Maybe.pure(3)

def plusOne(x: Int): Int = x + 1

m1.map(plusOne)

val m2 = Maybe.nothing()

m2.map(plusOne)

val absoluteM = Maybe.lift(math.abs)

absoluteM(Maybe.pure(-9))
absoluteM(Maybe.pure(12))
absoluteM(Maybe.nothing())










