// scala builtin Either type
import functional.part2.logging._

def pow2(a: Int): Int = a * a

def pow2WithLog(a: Int): Logging[Int] =
  Logging.log("before").map(
    _ => pow2(a)
  ).flatMap(
    r => Logging.log("after").map(
      _ => r
    )
  )

pow2WithLog(7)


// same with our new >>, << operators


def pow2WithLog2(a: Int): Logging[Int] =
  Logging.log("before") >> Logging.pure(pow2(a)) << Logging.log("after")


pow2WithLog(9)

// even nicer with for-yield
def pow2WithLog3(a: Int): Logging[Int] = for (
  _ <- Logging.log(s"before power2 of $a");
  r = pow2(a);
  _ <- Logging.log(s"after power2 of $a with result of $r")
) yield r

pow2WithLog3(6)

def plus3(a: Int, b: Int, c: Int): Int = a + b + c


Logging.map3(pow2WithLog3(2), pow2WithLog3(4), pow2WithLog3(5), plus3)


def sumPow(a: Int, b: Int, c: Int): Logging[Int] = for (
  _ <- Logging.log(s"before sum3: of $a, $b, $c");
  aa <- pow2WithLog3(a);
  bb <- pow2WithLog3(b);
  cc <- pow2WithLog3(c);
  r = plus3(aa, bb, cc);
  _ <- Logging.log(s"after sum3 of $aa, $bb, $cc with result of $r")
) yield r


sumPow(2, 3, 4).value
for (l <- sumPow(2, 3, 4).log) {
  println(l)
}


