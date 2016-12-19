package net.nicktelford.validation

import cats.data.Validated.{invalidNel, invalid, valid}
import cats.data.{NonEmptyList => NEL}
import org.scalatest._

import cats.instances.all._

object Person {
  import dsl._

  implicit val personValidator = ConstraintValidator.all[Person](
    require(_.name, notEmpty[String]),
    require(_.age, isPositive[Int])
  )
}

case class Person(name: String, age: Int)

class PersonTest extends FlatSpec with Matchers {

  val validator =

  "Validator" should "validate a valid Person" in {
    Person("Nick", 29).validated should be(valid(Person("Nick", 29)))
  }

  it should "yield an error when validating with no name" in {
    val person = Person("", 29)
    person.validated should be {
      invalidNel(ConstraintViolation(person, "name" :: Nil, "must not be empty"))
    }
  }

  it should "yield an error when validating with negative age" in {
    val person = Person("Nick", -1)
    person.validated should be {
      invalidNel(ConstraintViolation(person, "age" :: Nil, "must be positive"))
    }
  }

  it should "yield multiple errors when validating with multiple errors" in {
    val person = Person("", -1)
    person.validated should be {
      invalid(NEL.of(
        ConstraintViolation(person, "name" :: Nil, "must not be empty"),
        ConstraintViolation(person, "age" :: Nil, "must be positive")
      ))
    }
  }

  it should "leftMap to transform errors" in {
    val person = Person("", -1)
    ConstraintValidator[Person].leftMap(_.map(_.cause.length)).apply(person) should be {
      invalid(NEL.of(
        "must not be empty".length,
        "must be positive".length
      ))
    }
  }

  it should "provide the cartesian syntax" in {
    import cats.syntax.cartesian._
    import instances._
    import dsl._

    val nameValidator = require[Person](_.name, notEmpty[String])
    val ageValidator = require[Person](_.age, isPositive[Int])
    val validPerson = Person("Nick", 30)
    val invalidName = Person("", 20)
    val invalidAge = Person("Chris", -299)
    val invalidPerson = Person("", -1)

    val validator = (nameValidator |@| ageValidator)
      .imap { case (x, y) => x } { x => (x, x) }

    validator(validPerson) should be(valid(validPerson))
    validator(invalidName) should be {
      invalidNel(ConstraintViolation(invalidName, "name" :: Nil, "must not be empty"))
    }
    validator(invalidAge) should be {
      invalidNel(ConstraintViolation(invalidAge, "age" :: Nil, "must be positive"))
    }
    validator(invalidPerson) should be {
      invalid(NEL.of(
        ConstraintViolation(invalidPerson, "name" :: Nil, "must not be empty"),
        ConstraintViolation(invalidPerson, "age" :: Nil, "must be positive")
      ))
    }
  }
}
