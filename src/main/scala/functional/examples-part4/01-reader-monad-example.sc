import functional.part3.monad._
import functional.part4.readerMonad._

case class User(
  id: Int,
  name: String,
  addressId: Int
)

case class Address(
    id: Int,
    city: String,
    street: String,
    houseNumber: Int,
)

// Dummy database
// reality the database will not be pure and it's ok, the query construction will
// be pure, the query execution will not

class DataBase {

  val users: Map[Int, User] = Map(
    1 -> User(1, "John", 3),
    2 -> User(2, "Lilly", 2),
    3 -> User(3, "Jim", 1),
    4 -> User(4, "Barbara", 1)
  )

  val addresses: Map[Int, Address] = Map(
    1 -> Address(1, "Tel-Aviv", "Kaplan", 12),
    2 -> Address(2, "Haifa", "Hertzel", 7),
    3 -> Address(3, "Tel-Aviv", "Begin", 14)
  )

}

val db = new DataBase


// define alias for easier typing and better readability
type SQL[A] = Reader[DataBase, A]
def getDB: SQL[DataBase] = Reader.ask

def getUserById(userId: Int): SQL[User] = Reader(d => d.users(userId))
def getAddressById(addressId: Int): SQL[Address] = Reader(d => d.addresses(addressId))

// pure combination of operations
val getStreetOfUser2 =
  getUserById(2)
    .flatMap(u => getAddressById(u.addressId))
    .map(_.street)

// running doesn't have to be pure
getStreetOfUser2.run(db)

def getAddressFromUserId(userId: Int): SQL[String] =
  for (
    user <- getUserById(userId);
    address <- getAddressById(user.addressId);
    street = address.street
  ) yield street


getAddressFromUserId(2).run(db)

def shareAHouseAlone(userId1: Int, userId2: Int): SQL[Boolean] =
  for (
    user1 <- getUserById(userId1);
    user2 <- getUserById(userId2);
    db <- getDB; // just get the context here to do something with it in the computation
    numPeopleWithAddress = db.users.values.count(_.addressId == user1.addressId)
  ) yield user1.addressId == user2.addressId && numPeopleWithAddress == 2

shareAHouseAlone(1, 3).run(db)
shareAHouseAlone(3, 4).run(db)

def getUserIdByName(name: String): SQL[Int] = Reader(d => d.users.values.find(u => u.name == name).get.id)


val jimUserId = getUserIdByName("Jim")
val barbaraUserId = getUserIdByName("Barbara")

// tupled - converts functions like f(X,Y) => Z  -->  f((X, Y)) => Z
Monad[SQL].product(jimUserId, barbaraUserId).flatMap(shareAHouseAlone.tupled).run(db)


