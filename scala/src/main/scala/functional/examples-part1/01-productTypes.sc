// Tuples
val a = (1, 2)
val b = (9, 8)

// access the individual positional via the _x getters
a._1
b._2

// Type aliasing (for readability)
type TPoint = (Int, Int)

// _1, _2, ... to access specific positions in a tuple value
def plus(p1: TPoint, p2: TPoint): TPoint = (p1._1 + p2._1, p1._2 + p2._2)

plus(a, b)

// Case Classes ~= Named Tuples
case class Point(x: Int, y: Int) {

  // notice there is no 'new' used when constructing case classes
  def |+|(other: Point): Point = Point(this.x + other.x, this.y + other.y)

}

val p1 = Point(3, 7)
val p2 = Point(2, -2)

// accessing individual fields via the . operator
p1.x
p2.y

// in scala we can create operators with multiple symbols
p1 |+| p2