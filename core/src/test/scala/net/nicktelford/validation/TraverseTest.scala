package net.nicktelford.validation

import scala.language.higherKinds

import cats.implicits._
import cats.data.Validated.{invalidNel, invalid, valid}
import cats.data.{NonEmptyList => NEL}
import org.scalatest._

class TraverseTest extends FlatSpec with Matchers {

  "Lists" should "validate when empty" in {
    val list = List.empty[Person]
    list.validated should be(valid(list))
  }

  it should "validate with one valid element" in {
    val list = Person("Nick", 1) :: Nil
    list.validated should be(valid(list))
  }

  it should "fail with one valid element and one invalid element" in {
    val list = Person("Nick", 1) :: Person("Chris", -1) :: Nil
    list.validated should be {
      invalidNel(ConstraintViolation("age", "must be positive"))
    }
  }

  it should "fail with multiple elements, all invalid" in {
    val list = Person("", 1) :: Person("Chris", -1) :: Nil
    list.validated should be {
      invalid(NEL(
        ConstraintViolation("name", "must not be empty"),
        ConstraintViolation("age", "must be positive")
      ))
    }
  }

  "Option" should "validate when empty" in {
    val x = Option.empty[Person]
    x.validated should be(valid(x))
  }

  it should "validate with one valid element" in {
    val x = Option(Person("Nick", 1))
    x.validated should be(valid(x))
  }

  it should "fail with one invalid element" in {
    val x = Option(Person("Chris", -1))
    x.validated should be {
      invalidNel(ConstraintViolation("age", "must be positive"))
    }
  }
}
