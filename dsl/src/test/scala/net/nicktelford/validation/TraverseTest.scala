package net.nicktelford.validation

import scala.language.higherKinds

import cats.data.Validated.{invalidNel, invalid, valid}
import cats.data.{NonEmptyList => NEL}
import org.scalatest._

import cats.implicits._
class TraverseTest extends FlatSpec with Matchers {

  import instances._
  import cats.instances.all._

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
      invalidNel(ConstraintViolation(list, "age" :: Nil, "must be positive"))
    }
  }

  it should "fail with multiple elements, all invalid" in {
    val list = Person("", 1) :: Person("Chris", -1) :: Nil
    list.validated should be {
      invalid(NEL.of(
        ConstraintViolation(list, "name" :: Nil, "must not be empty"),
        ConstraintViolation(list, "age" :: Nil, "must be positive")
      ))
    }
  }

  "Map" should "validate when empty" in {
    val x = Map.empty[String, Person]
    x.validated should be(valid(x))
  }

  it should "validate with one valid element" in {
    val x = Map("Test" -> Person("Nick", 1))
    x.validated should be(valid(x))
  }

  it should "fail with one valid element and one invalid element" in {
    val x = Map("Valid" -> Person("Nick", 1), "Invalid" -> Person("Chris", -1))
    x.validated should be {
      invalidNel(ConstraintViolation(x, "age" :: Nil, "must be positive"))
    }
  }

  it should "fail with multiple elements, all invalid" in {
    val x = Map("Invalid1" -> Person("", 1), "Invalid2" -> Person("Chris", -1))
    x.validated should be {
      invalid(NEL.of(
        ConstraintViolation(x, "name" :: Nil, "must not be empty"),
        ConstraintViolation(x, "age" :: Nil, "must be positive")
      ))
    }
  }

  "Either" should "validate when left" in {
    val x = Either.left[Throwable, Person](new Throwable)
    x.validated should be(valid(x))
  }

  it should "validate with a valid right-side" in {
    val x = Either.right[Throwable, Person](Person("Nick", 1))
    x.validated should be(valid(x))
  }

  it should "fail with invalid right-side" in {
    val x = Either.right[Throwable, Person](Person("Nick", -1))
    x.validated should be {
      invalidNel(ConstraintViolation(x, "age" :: Nil, "must be positive"))
    }
  }

  it should "fail with invalid right-side, multiple violations" in {
    val x = Either.right[Throwable, Person](Person("", -1))
    x.validated should be {
      invalid(NEL.of(
        ConstraintViolation(x, "name" :: Nil, "must not be empty"),
        ConstraintViolation(x, "age" :: Nil, "must be positive")
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
      invalidNel(ConstraintViolation(x, "age" :: Nil, "must be positive"))
    }
  }
}
