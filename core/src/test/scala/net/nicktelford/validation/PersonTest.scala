package net.nicktelford.validation

import cats.implicits._
import cats.data.Validated.{invalid, valid}
import cats.data.{NonEmptyList => NEL}
import validation._
import org.scalatest._

object Person {
  implicit val personValidator = Validator[String, Person](
    require(_.name.nonEmpty, "name must not be empty"),
    require(_.age > 0, "age must be positive")
  )
}

case class Person(name: String, age: Int)

class PersonTest extends FlatSpec with Matchers {

  val validator = implicitly[Validator[String, Person]]

  "A Person being validated" should "validate a valid Person" in {
    validator.validate(Person("Nick", 29)) should be(valid(Person("Nick", 29)))
  }

  it should "yield an error when validating with no name" in {
    val person = Person("", 29)
    validator.validate(person) should be {
      invalid(ConstraintViolations(person, NEL("name must not be empty")))
    }
  }

  it should "yield an error when validating with negative age" in {
    val person = Person("Nick", -1)
    validator.validate(person) should be {
      invalid(ConstraintViolations(person, NEL("age must be positive")))
    }
  }

  it should "yield multiple errors when validating with multiple errors" in {
    val person = Person("", -1)
    validator.validate(person) should be {
      invalid(
        ConstraintViolations(
          person, NEL("name must not be empty","age must be positive")
        )
      )
    }
  }

  it should "leftMap to transform errors" in {
    val person = Person("", -1)
    validator.leftMap(_.length).validate(person) should be {
      invalid(ConstraintViolations(
        person, NEL("name must not be empty".length, "age must be positive".length)
      ))
    }
  }
}
