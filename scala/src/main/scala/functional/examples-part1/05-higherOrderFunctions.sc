import functional.part1.intList.*

val lst1 = IntCons(1, IntCons(2, (IntCons(3, IntNil))))

def map(f: (Int) => Int, lst: IntList): IntList = lst match {
    case IntNil => IntNil
    // Notice we have a recursion here
    case IntCons(h, t) => IntCons(f(h), map(f, t))
  }


map((x) => x*2, lst1)

// short lambda notation
map(_ * 2, lst1)

def filter(p: (Int) => Boolean, lst: IntList): IntList = lst match {
    case IntNil => IntNil
    // Notice we have a recursion here
    case IntCons(h, t) if p(h) => IntCons(h, filter(p, t))
    case IntCons(h, t) => filter(p, t)
  }

val odd: (Int) => Boolean = _ % 2 == 1
filter(odd, lst1)
