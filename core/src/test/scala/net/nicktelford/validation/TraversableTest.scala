package net.nicktelford.validation

import scala.language.higherKinds

import cats.implicits._
import cats.data.Validated.{invalidNel, invalid, valid}
import cats.data.{ValidatedNel, NonEmptyList => NEL}
import org.scalatest._
import cats.Traverse

class TraversableTest extends FlatSpec with Matchers {

  implicit def traversableValidator[F[_]: Traverse, E, A]
                                   (implicit Validator: Validator[E, A]) = {
    new Validator[E, F[A]] {
      override def validate(subject: F[A]): ValidatedNel[E, F[A]] = {
        Traverse[F].sequence[ValidatedNel[E, ?], A] {
          Traverse[F].map(subject)(Validator.validate)
        }
      }
    }
  }

  val validator = implicitly[Validator[ConstraintViolation, List[Person]]]

  "Lists" should "validate when empty" in {
    val list = List.empty[Person]
    validator.validate(list) should be(valid(list))
  }

  it should "validate with one valid element" in {
    val list = Person("Nick", 1) :: Nil
    validator.validate(list) should be(valid(list))
  }

  it should "fail with one valid element and one invalid element" in {
    val list = Person("Nick", 1) :: Person("Chris", -1) :: Nil
    validator.validate(list) should be {
      invalidNel(ConstraintViolation("age must be positive"))
    }
  }

  it should "fail with multiple elements, all invalid" in {
    val list = Person("", 1) :: Person("Chris", -1) :: Nil
    validator.validate(list) should be {
      invalid(NEL(
        ConstraintViolation("name must not be empty"),
        ConstraintViolation("age must be positive")
      ))
    }
  }

}
