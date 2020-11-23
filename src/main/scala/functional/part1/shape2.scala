package functional.part1

object shape2 {

  // trait is somewhat equivelent to Java interface
  // sealed traits can only be implemented in the same file, meaning the possible options is not extendable later
  enum Shape {
    case Square(x: Double, y: Double, width: Double, height: Double)
    case Circle(x: Double, y: Double, radius: Double)
  }

}

