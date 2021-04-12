// scala builtin Either type
import functional.part1.maybePart4._

type Email = String
type Name = String
case class User(
  name: Name,
  age: Maybe[Int] = Maybe.nothing(),
  email: Maybe[Email] = Maybe.nothing(),
  parentName: Maybe[Name] = Maybe.nothing()
)

def getUserByName(name: String, lst: List[User]): Maybe[User] = lst match {
  case Nil => Maybe.nothing()
  case user :: tail if user.name == name => Maybe.pure(user)
  case user :: tail => getUserByName(name, tail)
}

def getUserEmail(name: String, lst: List[User]): Maybe[Email] = {
  val user = getUserByName(name, lst)
  if(user.isDefined) {
    return user.get.email
  } else {
    return Maybe.nothing()
  }
}


val users = List(
  User("jim"),
  User("betty", Maybe.pure(70), Maybe.pure("betty@gmail.com"), Maybe.pure("paul")),
  User("john", Maybe.pure(30), Maybe.pure("john@aol.com"), Maybe.pure("betty")),
)


getUserEmail("betty", users)
getUserEmail("jim", users)
getUserEmail("paul", users)


def getParentEmail(userName: Name, lst: List[User]): Maybe[Email] = {
  val user = getUserByName(userName, lst)
  if(user.isDefined) {
    val parentName = user.get.parentName
    if(parentName.isDefined) {
      val parent = getUserByName(parentName.get, lst)
      if (parent.isDefined) {
        parent.get.email
      } else {
        Maybe.nothing()
      }
    } else {
      Maybe.nothing()
    }
  } else {
    Maybe.nothing()
  }
}

getParentEmail("john", users)
getParentEmail("betty", users)
getParentEmail("paul", users)

def older(user1Name: Name, user2Name: Name, lst: List[User]): Maybe[Boolean] = {
  val user1 = getUserByName(user1Name, lst)
  val user2 = getUserByName(user2Name, lst)
  if (user1.isDefined && user2.isDefined) {
    val user1Age = user1.get.age
    val user2Age = user2.get.age
    if (user1Age.isDefined && user2Age.isDefined) {
      Maybe.pure(
        user1Age.get > user2Age.get
      )
    } else {
      Maybe.nothing()
    }
  } else {
    Maybe.nothing()
  }
}

older("john", "betty", users)
older("betty", "john", users)
older("suzy", "betty", users)
older("jim", "john", users)
