import functional.part3.applicative.*
import functional.part3.monad.*
import functional.part3.monad.given
import functional.part4.eitherMonad.*

// The definitions for Arithmetic and the other functions
// aren't located here because of some currentl limitation of the worksheets
// so we defined them in the eitherMonad module

def safeDiv(a: Double, b: Double): Arithmetic[Double] = {
  if (b == 0) {
    fail("can't divide by zero")
  } else {
    success(a / b)
  }
}

safeDiv(5, 7)
safeDiv(5, 0)

def safeSqrt(v: Double): Arithmetic[Double] = {
  if (v < 0) {
    fail("can't square negative values, this will produce an imaginary number")
  } else {
    success(math.sqrt(v))
  }
}

safeSqrt(9)
safeSqrt(4)
safeSqrt(-4)


def squreDiv(a: Double, b: Double): Arithmetic[Double] = for {
  sa <- safeSqrt(a);
  sb <- safeSqrt(b);
  r <- safeDiv(sa, sb)
} yield r


squreDiv(9, 3)
squreDiv(-9, 3)
squreDiv(9, 0)
