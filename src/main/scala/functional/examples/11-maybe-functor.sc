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



// how do we adopt this to be safe? (assuming we don't care about complex numbers)
// assuming we don't want to deal with NaN
def solvePolynom2(a: Double, b: Double, c: Double): (Double, Double) = {
  val s = math.sqrt(math.pow(b,2) - 4 * a * c)
  val d = 2 * a

  val solution1 = (-b + s) / d
  val solution2 = (-b - s) / d

  (solution1, solution2)
}

def safe_sqrt(x: Double): Maybe[Double] = x match {
  case x if x < 0 => Maybe.nothing()
  case x => Maybe.pure(math.sqrt(x))
}


// with mapping, we only need to express what happens if it suceeds, no need to write the negative case
// in larger applications this saves a lot of boiler plate code
def safeSolvePolynom2(a: Double, b: Double, c: Double): Maybe[(Double, Double)] = {
  safe_sqrt(math.pow(b,2) - 4 * a * c).map(s => {
    val d = 2 * a

    val solution1 = (-b + s) / d
    val solution2 = (-b - s) / d

    (solution1, solution2)
  })
}

safeSolvePolynom2(4, 0, 0)
safeSolvePolynom2(1, 0, 8)








