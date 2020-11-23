// lambda functions
// The type is not needed and is only here for illustration
val addOne: (Int) => Int = (x) => x + 1

addOne(1)

val pow2 = (x: Int) => x * x

pow2(2)

// closure
val plus7 = {
  val seven = 7

  (x: Int) => seven + x
}

plus7(3)

// function composition, there is also compose, but the order is reversed
val composed = addOne.andThen(pow2)
//val composed = g.compose(f)

composed(1)

// partially applied functions (not to be confused with scalas partial functions)
val plus3 = (x: Int, y: Int, z: Int) => x + y + z

// mark arguments you want to keep as _
val plus2And7 = plus3(_, 7, _)

plus2And7(7, 8)

// currying 
val plus3Curried = plus3.curried

val add1 = plus3Curried(0)(1) // (Int) => (Int) => (Int) => Int

add1(7)

