val a: Null = null // the only value of the Null type

// this is valid for any type other than List[String], every possible method will result in NullPointerException anyway
val listA: List[String] = a


// ??? is a scala special value that throws Not Implement Exception and its of type Nothing
val b: Nothing = ???

// this is valid because we will never reach here, there is no such thing as a value of b
val listB: List[String] = b

