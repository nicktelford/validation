package net.nicktelford.validation

import cats.data.Validated.{invalidNel, invalid, valid}
import cats.data.{NonEmptyList => NEL}
import org.scalatest._

object ComplexPerson {
  implicit val addressValidator = Validator.of[Address](
    require(_.street.nonEmpty, "street", "must not be empty")
  )

  implicit val personValidator = Validator.of[ComplexPerson](
    require(_.name.nonEmpty, "name", "must not be empty"),
    require(_.age > 0, "age", "must be positive"),
    validate(_.address, "address")
  )
}

case class ComplexPerson(name: String, age: Int, address: Address)

case class Address(building: Either[String, Int], street: String, city: String, country: String)

class ComplexTest extends FlatSpec with Matchers {

  val validator = implicitly[Validator[ConstraintViolation, ComplexPerson]]

  "ComplexPerson being validated" should "validate a valid ComplexPerson" in {
    val person = ComplexPerson("Nick", 30, Address(Right(100), "Frostmourne Terrace", "Skylake", "Belgium"))
    person.validated should be(valid(person))
  }

  it should "fail on invalid root object" in {
    val person = ComplexPerson("", 30, Address(Right(100), "Frostmourne Terrace", "Skylake", "Belgium"))
    person.validated should be {
      invalidNel(ConstraintViolation("name", "must not be empty"))
    }
  }

  it should "fail on invalid nested object" in {
    val person = ComplexPerson("Nick", 30, Address(Right(100), "", "Skylake", "Belgium"))
    person.validated should be {
      invalidNel(ConstraintViolation(Root / "address" / "street", "must not be empty"))
    }
  }

  it should "fail on invalid root and nested objects" in {
    val person = ComplexPerson("Nick", 0, Address(Right(100), "", "Skylake", "Belgium"))
    person.validated should be {
      invalid(NEL.of(
        ConstraintViolation("age", "must be positive"),
        ConstraintViolation(Root / "address" / "street", "must not be empty")
      ))
    }
  }
}
