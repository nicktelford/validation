# Validation
_A small, easy to use validation library for functional Scala_

## Usage

_Note: Until a release is made for the first version, everything below is 
subject to change._

Add the dependency to your project:

```
"net.nicktelford" %% "validation" % "1.0.0"
```

Create `implicit` `Validator` instances for the domain classes you wish to 
validate.

```scala
import net.nicktelford.validation._

sealed trait Currency
case object USD extends Currency
case object GBP extends Currency

case class LineItem(id: Int, name: String, price: Long, currency: Currency)
case class User(id: Int, username: String, basket: List[LineItem])

implicit val lineItemValidator = Validator[LineItem](
  require(_.id > 0, "id", "must be positive"),
  require(_.name.nonEmpty, "name", "must be positive"),
  require(_.price, "price", "must be positive")
)

implicit val userValidator = Validator[User](
  require(_.id > 0, "id", "must be positive"),
  require(_.username.nonEmpty, "username", "must not be empty"),
  require(x => !x.username.contains(" "), "username", "must not contain spaces"),
  validate(_.basket, "basket")
)
```

Then simply validate with the implicit validator:

```scala
import cats.data.Validated.{Valid, Invalid}

def user: User

implicitly[Validator[ConstraintViolation, User]].validate(user) match {
  case Valid(user) => println("$user is valid!")
  case Invalid(violations) => violations.foreach(println)
}
```
