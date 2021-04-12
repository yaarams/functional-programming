import functional.part3.monad.*

def safeSqrt(x: Double): Option[Double] = {
  if (x >= 0) {
    Some(math.sqrt(x))
  } else {
    None
  }
}

def twiceSqrt(x: Double) =
  for(
    v1 <- safeSqrt(x);
    v2 <- safeSqrt(v1)
  ) yield v2

twiceSqrt(16)

// simpler and shorter
val twiceSqrt2 = safeSqrt >=> safeSqrt
twiceSqrt2(16)

