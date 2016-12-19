package net.nicktelford.validation

import cats.{Functor, Semigroup}
import cats.data.Validated
import cats.data.Validated.{invalid, valid}
import cats.data.{NonEmptyList => NEL}
import cats.syntax.cartesian._

object ConstraintValidator {

  type E = NEL[ConstraintViolation]

  def id[A]: ConstraintValidator[A] = Validator.id[E, A]
  def apply[A](implicit V: ConstraintValidator[A]): ConstraintValidator[A] = V
  def all[A](validators: ConstraintValidator[A]*): ConstraintValidator[A] =
    Validator.all(validators: _*)

  def apply[A](predicate: A => Boolean, msg: => String): ConstraintValidator[A] =
    Validator(predicate, sbj => NEL.of(ConstraintViolation(sbj, Nil, msg)))

  def apply[A, B](selector: A => B,
                  path: List[String],
                  validator: ConstraintValidator[B]): ConstraintValidator[A] =
    Validator(selector, path, validator, e => e.map { x => x.copy(path = path ++ x.path) })
}

object Validator {

  def id[E, A]: Validator[E, A] = new Validator[E, A] {
    def apply(subject: A) = valid(subject)
  }

  def apply[E, A](implicit V: Validator[E, A]): Validator[E, A] = V

  def all[E: Semigroup, A](validators: Validator[E, A]*): Validator[E, A] =
    validators.foldLeft(Validator.id[E, A]) { _ && _ }

  def apply[E, A](predicate: A => Boolean, error: A => E): Validator[E, A] =
    new Validator[E, A] {
      def apply(subject: A): Validated[E, A] = {
        if (predicate(subject))
          valid(subject)
        else
          invalid(error(subject))
      }
    }

  def apply[E, A, B](selector: A => B,
                     path: List[String],
                     validator: Validator[E, B],
                     mapError: E => E): Validator[E, A] =
    new Validator[E, A] {
      def apply(subject: A): Validated[E, A] = {
        validator(selector(subject)).bimap[E, A](mapError, _ => subject)
      }
    }
}

trait Validator[E, A] extends (A => Validated[E, A]) {
  self =>

  // allows combining of Validators
  def &&(other: Validator[E, A])(implicit E: Semigroup[E]): Validator[E, A] =
    new Validator[E, A] {
      def apply(subject: A): Validated[E, A] =
        (self.apply(subject) |@| other.apply(subject)).map { case (x, _) => x }
    }

  // from Cartesian
  def product[B](other: Validator[E, B])(implicit E: Semigroup[E]): Validator[E, (A, B)] =
    new Validator[E, (A, B)] {
      override def apply(subject: (A, B)): Validated[E, (A, B)] = {
        Validator.this.apply(subject._1).product(other.apply(subject._2))
      }
    }

  // from Invariant
  def imap[B](f: A => B)(g: B => A): Validator[E, B] = new Validator[E, B] {
    override def apply(subject: B): Validated[E, B] = {
      Validator.this.apply(g(subject)).map(f)
    }
  }

  def leftMap[EE](f: E => EE): Validator[EE, A] = new Validator[EE, A] {
    override def apply(v1: A): Validated[EE, A] = self.apply(v1).leftMap(f)
  }
}
