package net.nicktelford.validation

import scala.language.higherKinds

import cats.implicits._
import cats.data.Validated.{invalid, valid}
import cats.data.{ValidatedNel, NonEmptyList => NEL}
import validation._
import org.scalatest._
import cats.{Applicative, Traverse}

class ApplicativeTest extends FlatSpec with Matchers {

  implicit def applicativeValidator[F[_]: Applicative: Traverse, E, A](implicit Validator: Validator[E, A]) = {
    new Validator[E, F[A]] {
      override private[validation] def validateConstraints(subject: F[A]): ValidatedNel[E, F[A]] = {
        Traverse[F].sequence[ValidatedNel[E, ?], A](Traverse[F].map(subject)(Validator.validateConstraints))
      }
    }
  }

  val validator = implicitly[Validator[String, List[Person]]]

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
      invalid(ConstraintViolations(list, NEL("age must be positive")))
    }
  }

}
