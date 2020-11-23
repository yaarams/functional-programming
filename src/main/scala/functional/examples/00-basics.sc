// variable declaration

val x: Int = 7 // the Int here is redundent because 7 is an Int Literal
val y: Int = 9 // the Int here is redundent because 9 is an Int Literal


// simple pure function declaration
def plus3(x: Int, y: Int, z: Int): Int = x + y + z

// running functions
plus3(x, 5, y)

// if statement (expression and not a statement)
val r = if(x > 4) {
  11
} else {
  12
}

// function with if example
def absolute(x: Int): Int = if (x < 0) -x else x
absolute(7)
absolute(-7)

// code blocks
val blockResult = {
  val x = 2;
  val y = 7;

  plus3(y, 5, x)
}

blockResult + 1

// functions with long body are just code blocks
def max3(x: Int, y: Int, z: Int): Int = {
  val maxFirstTwo = if (x > y) {
    x
  } else {
    y
  }

  // last expression in block will be returned
  if(maxFirstTwo > z) {
    maxFirstTwo
  } else {
    z
  }
}

max3(-4, 8, 5)


