package net.nicktelford.validation

import cats.implicits._
import cats.data.Validated.{invalid, valid}
import cats.data.{NonEmptyList => NEL}
import validation._
import org.scalatest._

object ComplexPerson {
  implicit val addressValidator = Validator[String, Address](
    require(_.street.nonEmpty, "street must not be empty")
  )

  implicit val personValidator = Validator[String, ComplexPerson](
    require(_.name.nonEmpty, "name must not be empty"),
    require(_.age > 0, "age must be positive"),
    validate(_.address)
  )
}

case class ComplexPerson(name: String, age: Int, address: Address)

case class Address(building: Either[String, Int], street: String, city: String, country: String)

class ComplexTest extends FlatSpec with Matchers {

  val validator = implicitly[Validator[String, ComplexPerson]]

  "ComplexPerson being validated" should "validate a valid ComplexPerson" in {
    val person = ComplexPerson("Nick", 30, Address(Right(100), "Frostmourne Terrace", "Skylake", "Belgium"))
    validator.validate(person) should be(valid(person))
  }

  it should "fail on invalid root object" in {
    val person = ComplexPerson("", 30, Address(Right(100), "Frostmourne Terrace", "Skylake", "Belgium"))
    validator.validate(person) should be {
      invalid(ConstraintViolations(person, NEL("name must not be empty")))
    }
  }

  it should "fail on invalid nested object" in {
    val person = ComplexPerson("Nick", 30, Address(Right(100), "", "Skylake", "Belgium"))
    validator.validate(person) should be {
      invalid(ConstraintViolations(person, NEL("street must not be empty")))
    }
  }

  it should "fail on invalid root and nested objects" in {
    val person = ComplexPerson("Nick", 0, Address(Right(100), "", "Skylake", "Belgium"))
    validator.validate(person) should be {
      invalid(ConstraintViolations(person, NEL("age must be positive", "street must not be empty")))
    }
  }
}
