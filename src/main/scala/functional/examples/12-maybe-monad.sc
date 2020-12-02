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



def safe_sqrt(x: Double): Maybe[Double] = x match {
  case x if x < 0 => Maybe.nothing()
  case x => Maybe.pure(math.sqrt(x))
}

// This barely different than the not safe version but we get the safety
def safeSolvePolynom2(a: Double, b: Double, c: Double): Maybe[(Double, Double)] =
  for (
    s <- safe_sqrt(math.pow(b,2) - 4 * a * c);
    d = 2 * a;
    solution1 = (-b + s) / d;
    solution2 = (-b - s) / d
  ) yield (solution1, solution2)

safeSolvePolynom2(4, 0, 0)
safeSolvePolynom2(1, 0, 8)
