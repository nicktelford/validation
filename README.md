# Validation

_A functional library for runtime validation of domain objects._

## Overview

_Note: Until a release is made for the first version, everything is subject to 
change._

The goal of this project is to provide a small, simple but powerful API to 
enable runtime validation of domain objects.

Currently, this project is built with/against 
[cats](http://github.com/typelevel/cats), however, a port to
[Scalaz](http://github.com/scalaz/scalaz) is planned.

## Getting Started

Validation is currently built for Scala 2.10 and 2.11.

Add the dependency to your project:

```
"net.nicktelford" %% "validation" % "1.0.0"
```

### Usage Example

Before writing our validations, we need a domain model to validate:

```scala
sealed trait Currency
case object USD extends Currency
case object GBP extends Currency

case class LineItem(id: Int, name: String, price: Long, currency: Currency)
case class User(id: Int, username: String, basket: List[LineItem])
```

Notice how `currency` is adequately described by its types. Whenever possible, 
you should strive to use the type system to guarantee as much as possible about
your domain models.

However, sometimes this is not enough, and elements of your domain models will
use a type that doesn't entirely capture the constraints of your domain; for 
these, we define our validations.

Create `implicit` `Validator` instances for the domain classes you wish to 
validate.

```scala
import net.nicktelford.validation._

implicit val lineItemValidator = Validator.of[LineItem](
  require(_.id > 0, "id", "must be positive"),
  require(_.name.nonEmpty, "name", "must be positive"),
  require(_.price, "price", "must be positive")
)

implicit val userValidator = Validator.of[User](
  require(_.id > 0, "id", "must be positive"),
  require(_.username.nonEmpty, "username", "must not be empty"),
  require(x => !x.username.contains(" "), "username", "must not contain spaces"),
  validate(_.basket, "basket")
)
```

To validate that an instance of our model adheres to the constraints we've
specified, call the implicit `validated` method available on your model:

```scala
import cats.data.ValidatedNel

val user: User = ???

val validated: ValidatedNel[ConstraintViolation, User] = user.validated
```

The result is a [cats ValidatedNel](http://typelevel.org/cats/tut/validated.html)
value, which allows you to match on and combine multiple results easily.

```scala
import cats.data.Validated.{Valid, Invalid}

user.validated match {
  case Valid(user) => println(s"$user is valid!")
  case Invalid(violations) => violations.foreach(println)
}
```

The error type (left-side of the `ValidatedNel`) is `ConstraintViolation` by
default. `ConstraintViolation` captures the reasons for validation failure, with
each `ConstraintViolation` capturing a single constraint violation.

A `ConstraintViolation` consists of:

  * `cause: String` - a message describing the failure; in the above example, 
    this is the third argument to `require` for each constraint.
  * `path: List[String]` - the "path" to the node in the object graph for which 
    this violation applies to.
  * `message: String` - a combination of `path` and `cause` to yield an error
    message uniquely describing this violation.

`ConstraintViolations` can also be easily matched on, but they are mostly used
to provide an error message to the user:

```scala
def generateId(): Int = ???

user.validated match {
  case Valid(user) => user
  case Invalid(OneAnd(ConstraintViolation("id" :: Nil, _), Nil)) =>
    user.copy(id = generateId)
  case Invalid(violations) =>
    throw new RuntimeException(violations.map(_.message).unwrap.mkString("; "))
}
```

## API Documentation

Validators are defined using one of two functions:

  * `Validator.of[A](constraints: Validator[ConstraintViolation, A]*)`:
   
    Creates a `Validator` with a set of `constraints`, using the default error
    type, `ConstraintViolation`.
    
  * `Validator.custom[E, A](constraints: Validator[E, A]*)`:
  
    Creates a `Validator` with a set of `constraints`, using a custom error 
    type, `E`. When using this, the third parameter of your constraints take a
    function from the object being validated, and the path within the graph the 
    object is defined at to your custom error type: `(A, List[String]) => E`.
    
Constraints are specified using the following functions:

  * `require[A](predicate: A => Boolean,
                name: => String,
                msg: String): Validator[ConstraintViolation, A]`:
                
    * `predicate`: the predicate that validates the constraint.
    * `name`: the name of this property; usually the name of the node in the
      object graph.
    * `msg`: the message to use if the constraint is violated.
    
  * `constraint[E, A](predicate: A => Boolean,
                      name: String,
                      error: (A, List[String]) => E): Validator[E, A]`:

    * `predicate`: the predicate that validates the constraint.
    * `name`: the name of this property; usually the name of the node in the
      object graph.
    * `error`: a function that produces an instance of the error type if the
      constraint is violated. The inputs to this function are the object being 
      validated (type `A`) and the path to the object in the object graph (type
      `List[String]`).

  * `constraint[E, A, B](selector: A => B, name: => String): Validator[E, B]`:

    * `selector`: a function to select a nested object.
    * `name`: the name of this property; usually the name of the node in the
      object graph.
      
  * `validate[E, A, B](selector: A => B, name: => String): Validator[E, B]`:
  
    An alias for `constraint(selector, name)`, documented above.

Constraints are really just `Validator` instances, so you can combine them in 
any way you wish. The constructors defined above differentiate between
"validators" and "constraints" at the superficial level only, in order to 
provide an intuitive DSL.

## Copyright and License

Copyright &copy; 2016 Nick Telford
