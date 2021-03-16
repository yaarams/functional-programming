import functional.part1.shape1._

// accessing the values in a union through pattern matching (the match keyword)
def area(shape: Shape): Double = shape match {
    // variables we don't want to bind, use _
    case Circle(_, _, r) => r *r * math.Pi
    case Square(_, _, w, h) => w * h
  }

area(Circle(0, 0, 2))
area(Square(0, 0, 4, 5)) 


def representation(shape: Shape): String = shape match {
    case Circle(x, y, r) => s"Circle at ($x, $y) of radius $r"
    case Square(x, y, w, h) => s"Square at ($x, $y) with width: $w and height $h"
}

representation(Circle(1, 2, 3))

// pattern matching with guards (looks a bit more like a mathematical definition)
def abs(x: Int): Int = x match {
  case x if x < 0 => -x
  case _ => x
}

abs(-3)
abs(3)