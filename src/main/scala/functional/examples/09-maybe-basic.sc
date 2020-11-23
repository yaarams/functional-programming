import functional.part1.maybeApplicative._

def safeDiv(x: Int, y: Int): Maybe[Int] = y match {
  case 0 => Maybe.nothing()
  case y => Just(x / y)
}

safeDiv(12, 4)
safeDiv(12, 0)




