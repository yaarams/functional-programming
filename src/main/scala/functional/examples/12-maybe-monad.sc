import functional.part1.maybeMonad._

val m1 = Maybe.pure(2)
val m2 = Maybe.pure(7)
val m3 = Maybe.pure(4)


def plus3(x: Int, y: Int, z: Int): Int = x + y + z

Maybe.map3(m1, m2, m3, plus3)

// alternative to map3
val a = m1.flatMap(a => m2.flatMap(b => m3.map(c => plus3(a, b, c))))

// special language construct the for yield
val b = for {
  a <- m1;
  b <- m2;
  c <- m3
} yield {
  plus3(a,b, c)
}



