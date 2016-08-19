package net.nicktelford.validation

import cats.data.Validated.{invalidNel, invalid, valid}
import cats.data.{NonEmptyList => NEL}
import validation._
import org.scalatest._

object Person {
  implicit val personValidator = Validator[Person](
    require(_.name.nonEmpty, "name must not be empty"),
    require(_.age > 0, "age must be positive")
  )
}

case class Person(name: String, age: Int)

class PersonTest extends FlatSpec with Matchers {

  val validator = implicitly[Validator[ConstraintViolation, Person]]

  "Validator" should "validate a valid Person" in {
    validator.validate(Person("Nick", 29)) should be(valid(Person("Nick", 29)))
  }

  it should "yield an error when validating with no name" in {
    val person = Person("", 29)
    validator.validate(person) should be {
      invalidNel(ConstraintViolation("name must not be empty"))
    }
  }

  it should "yield an error when validating with negative age" in {
    val person = Person("Nick", -1)
    validator.validate(person) should be {
      invalidNel(ConstraintViolation("age must be positive"))
    }
  }

  it should "yield multiple errors when validating with multiple errors" in {
    val person = Person("", -1)
    validator.validate(person) should be {
      invalid(NEL(
        ConstraintViolation("name must not be empty"),
        ConstraintViolation("age must be positive")
      ))
    }
  }

  it should "leftMap to transform errors" in {
    val person = Person("", -1)
    validator.mapErrors(_.cause.length).validate(person) should be {
      invalid(NEL(
        "name must not be empty".length,
        "age must be positive".length
      ))
    }
  }

  it should "provide the cartesian syntax" in {
    import cats.syntax.cartesian._

    val nameValidator = Validator[Person](require(_.name.nonEmpty, "name must not be empty"))
    val ageValidator = Validator[Person](require(_.age >= 0, "age must be positive"))
    val validPerson = Person("Nick", 30)
    val invalidName = Person("", 20)
    val invalidAge = Person("Chris", -299)
    val invalidPerson = Person("", -1)

    val validator = (nameValidator |@| ageValidator)
      .imap((x, y) => x)(z => (z, z))

    validator.validate(validPerson) should be(valid(validPerson))
    validator.validate(invalidName) should be {
      invalidNel(ConstraintViolation("name must not be empty"))
    }
    validator.validate(invalidAge) should be {
      invalidNel(ConstraintViolation("age must be positive"))
    }
    validator.validate(invalidPerson) should be {
      invalid(NEL(
        ConstraintViolation("name must not be empty"),
        ConstraintViolation("age must be positive")
      ))
    }
  }
}
