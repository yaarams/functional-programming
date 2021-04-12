package functional.part1

object shape1 {

  // trait is somewhat equivelent to Java interface
  // sealed traits can only be implemented in the same file, meaning the possible options is not extendable later
  sealed trait Shape

  case class Square(x: Double, y: Double, width: Double, height: Double) extends Shape 

  case class Circle(x: Double, y: Double, radius: Double) extends Shape 

}

