// scala builtin Either type


// a useful function for terminating programs
def halt(): Nothing = System.exit(0).asInstanceOf[Nothing]


// a strict version (both the 'then' and the 'else' will be evaluated)

val x = 7
val y = 18

def if2Strict[A](cond: Boolean, ifTrue: A, ifFalse: A): A = if(cond) ifTrue else ifFalse


if2Strict(x > y, "X is bigger than Y", "X is not bigger than Y")

// we will halt even if though this branch isn't taken with this condition
if2Strict(y > x, "Y is bigger than X", halt()) // comment this out


// a version using thunks (avoid evaluating the expression not taken by passing functions)

def if2Thunk[A](cond: Boolean, ifTrue: () => A, ifFalse: () => A): A =
  if(cond) ifTrue() else ifFalse()

// we will NOT halt because if2 will not evaluate that branch
if2Thunk(x > y, halt, () => "X is not bigger than Y")


// a version using lazy

def if2Lazy[A](cond: Boolean, ifTrue: => A, ifFalse: => A): A = {
  // always assign non strict arguments into lazy vals
  lazy val t = ifTrue
  lazy val f = ifFalse
  if (cond) t else f
}

if2Lazy(x > y, "X is bigger than Y", "X is not bigger than Y")
if2Lazy(y > x, "Y is bigger than X", halt())


