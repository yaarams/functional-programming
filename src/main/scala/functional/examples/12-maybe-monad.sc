import functional.part1.maybeApplicative._

val m1 = Maybe.pure(3)
val m2 = Maybe.pure(7)
val m3 = Maybe.pure(4)


def plus3(x: Int, y: Int, z: Int): Int = x + y + z

Maybe.map3(m1, m2, m3, plus3)


// TODO: show for yield construt

