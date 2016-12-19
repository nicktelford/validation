package net.nicktelford.validation

import cats.data.Validated.{invalidNel, invalid, valid}
import cats.data.{NonEmptyList => NEL}
import cats.instances.all._
import org.scalatest._

object ComplexPerson {

  import dsl._

  implicit val addressValidator = ConstraintValidator.all[Address](
    require(_.street, notEmpty[String]),
    require(_.building, whenRight(isPositive[Int]))
  )

  implicit val personValidator = ConstraintValidator.all[ComplexPerson](
    require(_.name, notEmpty[String]),
    require(_.age, isPositive[Int]),
    validate(_.address)
  )
}

case class ComplexPerson(name: String, age: Int, address: Address)

case class Address(building: Either[String, Int], street: String, city: String, country: String)

class ComplexTest extends FlatSpec with Matchers {

  val validator = implicitly[ConstraintValidator[ComplexPerson]]

  "ComplexPerson being validated" should "validate a valid ComplexPerson" in {
    val person = ComplexPerson("Nick", 30, Address(Right(100), "Frostmourne Terrace", "Skylake", "Belgium"))
    person.validated should be(valid(person))
  }

  it should "fail on invalid root object" in {
    val person = ComplexPerson("", 30, Address(Right(100), "Frostmourne Terrace", "Skylake", "Belgium"))
    person.validated should be {
      invalidNel(ConstraintViolation(person, "name" :: Nil, "must not be empty"))
    }
  }

  it should "fail on invalid nested object" in {
    val person = ComplexPerson("Nick", 30, Address(Right(100), "", "Skylake", "Belgium"))
    person.validated should be {
      invalidNel(ConstraintViolation(person, "address" :: "street" :: Nil, "must not be empty"))
    }
  }

  it should "fail on invalid root and nested objects" in {
    val person = ComplexPerson("Nick", 0, Address(Right(100), "", "Skylake", "Belgium"))
    person.validated should be {
      invalid(NEL.of(
        ConstraintViolation(person, "age" :: Nil, "must be positive"),
        ConstraintViolation(person, "address" :: "street" :: Nil, "must not be empty")
      ))
    }
  }
}
