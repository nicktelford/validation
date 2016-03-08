package net.nicktelford.validation

import cats.data.Validated.{valid, invalidNel}
import org.scalatest._

class PersonTest extends FlatSpec with Matchers {

  case class Person(name: String, age: Int)
                   (implicit ctx: ValidationContext = ValidationContext.require) {
    constraint(name.nonEmpty, "name must not be empty")
    constraint(age > 0, "age must be a positive integer")
  }

  "A Person (without validation) " should "do nothing when valid" in {
    Person("Nick", 29) should be(
      Person("Nick", 29)
    )
  }

  it should "throw an IllegalArgumentException for empty name" in {
    an [IllegalArgumentException] should be thrownBy {
      Person("", 100)
    }
  }

  it should "throw an IllegalArgumentException for invalid age" in {
    an [IllegalArgumentException] should be thrownBy {
      Person("Fred", -1)
    }
  }

  "A Person (without macros) " should "validate when valid" in {
    implicit val ctx = ValidationContext.validated
    ctx.validate(Person("Nick", 29)) should be(
      valid(Person("Nick", 29))
    )
  }

  it should "yield an error for empty name" in {
    implicit val ctx = ValidationContext.validated
    ctx.validate(Person("", 100)) should be(
      invalidNel(ConstraintViolation("name must not be empty"))
    )
    ctx.violations should not be empty
  }

  it should "yield an error for invalid age" in {
    implicit val ctx = ValidationContext.validated
    ctx.validate(Person("Fred", -1)) should be (
      invalidNel(ConstraintViolation("age must be a positive integer"))
    )
    ctx.violations should not be empty
  }

  "A Person (macros) " should "validate when valid" in {
    validated(Person("Nick", 29)) should be(
      valid(Person("Nick", 29))
    )
  }

  it should "yield an error for empty name" in {
    validated(Person("", 100)) should be (
      invalidNel(ConstraintViolation("name must not be empty"))
    )
  }

  it should "yield an error for invalid age" in {
    validated(Person("Fred", -1)) should be (
      invalidNel(ConstraintViolation("age must be a positive integer"))
    )
  }
}
