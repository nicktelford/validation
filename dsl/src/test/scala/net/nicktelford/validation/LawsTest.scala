package net.nicktelford.validation

import cats.implicits._
import cats.Eq
import cats.data.NonEmptyList
import cats.data.Validated.{invalid, valid}
import cats.kernel.laws.GroupLaws
import cats.laws.discipline.{CartesianTests, InvariantTests}
import org.scalatest.FunSuite
import cats.laws.discipline.arbitrary._
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.typelevel.discipline.scalatest.Discipline

class LawsTest extends FunSuite with Discipline {

  import instances._

  implicit val constraintViolationArbitrary: Arbitrary[ConstraintViolation] = {
    import Arbitrary.arbitrary
    Arbitrary(
      for {
        path <- arbitrary[List[String]]
        cause <- arbitrary[String]
        subject <- arbitrary[Option[String]]
      } yield ConstraintViolation(path, cause, subject)
    )
  }

  implicit def validatorArbitrary[E, A](implicit E: Arbitrary[E]): Arbitrary[Validator[E, A]] = {
    def invalidValidator(e: E): Validator[E, A] = new Validator[E, A] {
      override def apply(subject: A) = invalid(e)
    }

    def validValidator: Validator[E, A] = new Validator[E, A] {
      override def apply(subject: A) = valid(subject)
    }

    Arbitrary(Gen.oneOf(
      E.arbitrary.map(invalidValidator),
      Gen.const(validValidator)
    ))
  }

  implicit val eqC: Eq[ConstraintViolation] =
    (x: ConstraintViolation, y: ConstraintViolation) => x == y

  implicit def eq[E, A](implicit EqE: Eq[E],
                                 EqA: Eq[A],
                                 ArbA: Arbitrary[A]): Eq[Validator[E, A]] =
    (x: Validator[E, A], y: Validator[E, A]) => {
      // note: this is a bit of a cheat, since there's no reasonable Eq instance
      // for Validator (nor Function1 for that matter), but we need one for law
      // checking
      Prop
        .forAll { a: A => x(a) === y(a) }
        .apply(Gen.Parameters.default).success
    }

  implicit val iso = CartesianTests.Isomorphisms.invariant[ConstraintValidator]
  checkAll("Validator[Int]", CartesianTests[ConstraintValidator].cartesian[Int, Int, Int])
  checkAll("Validator[Int]", InvariantTests[ConstraintValidator].invariant[Int, Int, Int])

  checkAll("Validator[Int]", GroupLaws[ConstraintValidator[Int]].monoid)

}
