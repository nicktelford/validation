package net.nicktelford.validation

import cats.implicits._
import cats.Eq
import cats.data.{NonEmptyList, ValidatedNel}
import cats.data.Validated.{invalidNel, valid}
import cats.kernel.laws.GroupLaws
import cats.laws.discipline.{CartesianTests, InvariantTests, MonoidKTests}
import org.scalatest.FunSuite
import cats.laws.discipline.arbitrary._
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.typelevel.discipline.scalatest.Discipline
import validation._

class LawsTest extends FunSuite with Discipline {

  implicit def validatorArbitrary[E, A](implicit E: Arbitrary[E]): Arbitrary[Validator[E, A]] = {
    def invalidValidator[E, A](e: E) = new Validator[E, A] {
      override def validate(subject: A): ValidatedNel[E, A] = invalidNel(e)
    }

    def validValidator[E, A] = new Validator[E, A] {
      override def validate(subject: A): ValidatedNel[E, A] = valid(subject)
    }

    Arbitrary(Gen.oneOf(
      E.arbitrary.map(invalidValidator[E, A]),
      Gen.const(validValidator[E, A])
    ))
  }

  implicit def eq[E, A](implicit EqE: Eq[E],
                        EqA: Eq[A],
                        EqEE: Eq[NonEmptyList[E]],
                        ArbA: Arbitrary[A]): Eq[Validator[E, A]] = new Eq[Validator[E, A]] {
    override def eqv(x: Validator[E, A], y: Validator[E, A]): Boolean = {
      // note: this is a bit of a cheat, since there's no reasonable Eq instance
      // for Validator (nor Function1 for that matter), but we need one for law
      // checking
      Prop
        .forAll { a: A => x.validate(a) === y.validate(a) }
        .apply(Gen.Parameters.default).success
    }
  }

  implicit val iso = CartesianTests.Isomorphisms.invariant[Validator[String, ?]]
  checkAll("Validator[String, Int]", CartesianTests[Validator[String, ?]].cartesian[Int, Int, Int])
  checkAll("Validator[String, Int]", InvariantTests[Validator[String, ?]].invariant[Int, Int, Int])

  checkAll("Validator[String, Int]", GroupLaws[Validator[String, Int]].monoid)

}
