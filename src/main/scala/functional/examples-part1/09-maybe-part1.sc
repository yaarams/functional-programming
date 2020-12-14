import functional.part1.maybePart1._

def safeDiv(x: Int, y: Int): Maybe[Int] = y match {
  case 0 => Maybe.nothing()
  case y => Just(x / y)
}

safeDiv(12, 4)
safeDiv(12, 0)

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

// This works but it's not much different from using null -> annoying, can we do better?
def safeSolvePolynom2(a: Double, b: Double, c: Double): Maybe[(Double, Double)] = {
  val s = safe_sqrt(math.pow(b,2) - 4 * a * c)
  if (s.isDefined) {
    val d = 2 * a

    val solution1 = (-b + s.get) / d
    val solution2 = (-b - s.get) / d

    Maybe.pure((solution1, solution2))
  } else {
    Maybe.nothing()
  }
}


safeSolvePolynom2(4, 0, 0)
safeSolvePolynom2(1, 0, 8)

