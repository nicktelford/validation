package net.nicktelford

import cats.implicits._
import cats.{Cartesian, Show, Traverse}
import cats.data.ValidatedNel
import cats.data.Validated.valid
import cats.functor.Invariant
import cats.kernel.Monoid

import scala.language.higherKinds

package object validation extends ConstraintViolation.Implicits {

  def require[A](predicate: A => Boolean,
                 name: String,
                 error: => String): Validator[ConstraintViolation, A] =
    constraint(
      predicate, name, (x: A, loc: Node) => ConstraintViolation(loc, error))

  def constraint[E, A](predicate: A => Boolean,
                       name: String,
                       error: (A, Node) => E): Validator[E, A] =
    new ConstraintValidator(predicate, error, Root / name)

  def constraint[E, A, B](selector: A => B, name: String)
                         (implicit V: Validator[E, B]): Validator[E, A] =
    validate(selector, name)(V)

  def validate[E, A, B](selector: A => B, name: String)
                       (implicit V: Validator[E, B]): Validator[E, A] =
    new NestedValidator(selector, Root / name)(V)

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
    new TraverseValidator[E, F, A]
  }

  class TraverseValidator[E, F[_]: Traverse, A](implicit V: Validator[E, A])
    extends Validator[E, F[A]] {

    override def parent(parents: Node): Validator[E, F[A]] =
      new TraverseValidator[E, F, A] {
        override private[validation] val path: Node =
          TraverseValidator.this.path.parent(parents)
      }

    override def validate(subject: F[A]): ValidatedNel[E, F[A]] = {
      Traverse[F].sequence[ValidatedNel[E, ?], A] {
        var i = 0
        Traverse[F].map(subject) { x =>
          val r = V.parent(V.path /# i).validate(x)
          i = i + 1
          r
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
