
def factorialRec(n: Int): Int = n match { 
  // avoid deling with undefined values for factorial of negatives
  case x if x <= 1 => 1 
  case x => factorialRec(x - 1) * x
}

factorialRec(5)


// factorial with tail recursion 
def factorial(n: Int): Int = {
  @annotation.tailrec
  def go(n: Int, acc: Int): Int = {
    if (n <= 0) acc else go(n-1, n * acc)
  }

  go(n, 1)
}


factorial(5)