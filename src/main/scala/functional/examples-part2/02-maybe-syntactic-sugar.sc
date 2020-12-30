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

def getUserEmail(name: String, lst: List[User]): Maybe[Email] =
  for(
    user <- getUserByName(name, lst);
    email <- user.email
  ) yield email


val users = List(
  User("jim"),
  User("betty", Maybe.pure(70), Maybe.pure("betty@gmail.com"), Maybe.pure("paul")),
  User("john", Maybe.pure(30), Maybe.pure("john@aol.com"), Maybe.pure("betty")),
)


getUserEmail("betty", users)
getUserEmail("jim", users)
getUserEmail("paul", users)


def getParentEmail(userName: Name, lst: List[User]): Maybe[Email] =
  for(
    user <- getUserByName(userName, lst);
    parentName <- user.parentName;
    parentUser <- getUserByName(parentName, lst);
    parentEmail <- parentUser.email
  ) yield parentEmail

getParentEmail("john", users)
getParentEmail("betty", users)
getParentEmail("paul", users)


def older(user1Name: Name, user2Name: Name, lst: List[User]): Maybe[Boolean] = {
  val user1Age = getUserByName(user1Name, lst).flatMap(u1 => u1.age)
  val user2Age = getUserByName(user2Name, lst).flatMap(u2 => u2.age)

  Maybe.map2(user1Age, user2Age, _ > _)
}


older("john", "betty", users)
older("betty", "john", users)
older("suzy", "betty", users)
older("jim", "john", users)

def older2(user1Name: Name, user2Name: Name, lst: List[User]): Maybe[Boolean] =
  for (
    user1 <- getUserByName(user1Name, lst);
    user1Age <- user1.age;
    user2 <- getUserByName(user2Name, lst);
    user2Age <- user2.age
  ) yield user1Age > user2Age

older2("john", "betty", users)
older2("betty", "john", users)
older2("suzy", "betty", users)
older2("jim", "john", users)

