package net.nicktelford

import cats.implicits._
import cats.{Cartesian, Show, Traverse}
import cats.data.ValidatedNel
import cats.data.Validated.valid
import cats.functor.Invariant
import cats.kernel.Monoid

import scala.language.higherKinds

package object validation {

  type Path = List[String]

  def require[A](predicate: A => Boolean,
                 name: String,
                 error: => String): Validator[ConstraintViolation, A] =
    constraint(
      predicate, name, (x: A, loc: Path) => ConstraintViolation(loc, error))

  def constraint[E, A](predicate: A => Boolean,
                       name: String,
                       error: (A, List[String]) => E): Validator[E, A] =
    new ConstraintValidator(predicate, error, name :: Nil)

  def constraint[E, A, B](selector: A => B, name: String)
                         (implicit V: Validator[E, B]): Validator[E, A] =
    validate(selector, name)(V)

  def validate[E, A, B](selector: A => B, name: String)
                       (implicit V: Validator[E, B]): Validator[E, A] =
    new NestedValidator(selector, name :: Nil)(V)

  implicit class ValidatorSyntax[A](val x: A) extends AnyVal {
    def validated(implicit V: Validator[ConstraintViolation, A]): ValidatedNel[ConstraintViolation, A] =
      V.validate(x)
  }

  implicit def validatorInstances1[E] =
    new Cartesian[Validator[E, ?]] with Invariant[Validator[E, ?]] {
      override def product[A, B](fa: Validator[E, A],
                                 fb: Validator[E, B]): Validator[E, (A, B)] =
        fa product fb

      override def imap[A, B](fa: Validator[E, A])
                             (f: (A) => B)
                             (g: (B) => A): Validator[E, B] = fa.imap(f)(g)
    }

  implicit def validatorInstances[E, A] =
    new Monoid[Validator[E, A]] {
      override def empty: Validator[E, A] = new Validator[E, A] {
        override def validate(subject: A): ValidatedNel[E, A] = valid(subject)
      }

      override def combine(x: Validator[E, A],
                           y: Validator[E, A]): Validator[E, A] = x combine y
    }

  implicit val constraintViolationInstance =
    Show.show[ConstraintViolation](_.message)

  implicit def traversableValidator[F[_]: Traverse, E, A]
                                   (implicit V: Validator[E, A]) = {
    new Validator[E, F[A]] {
      override def validate(subject: F[A]): ValidatedNel[E, F[A]] = {
        Traverse[F].sequence[ValidatedNel[E, ?], A] {
          Traverse[F].map(subject)(V.validate)
        }
      }
    }
  }

  implicit def traversable2Validator[F[_, _], E, A, B](implicit T: Traverse[F[A, ?]],
                                                                V: Validator[E, B]) =
    new Validator[E, F[A, B]] {
      override def validate(subject: F[A, B]): ValidatedNel[E, F[A, B]] = {
        Traverse[F[A, ?]].sequence[ValidatedNel[E, ?], B] {
          Traverse[F[A, ?]].map(subject)(V.validate)
        }
      }
    }
}
