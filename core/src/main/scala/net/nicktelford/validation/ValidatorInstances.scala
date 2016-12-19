package net.nicktelford.validation

import scala.language.higherKinds
import cats.data.Validated
import cats.functor.Invariant
import cats.{Cartesian, Monoid, Semigroup, Traverse}

trait ValidatorInstances {

  implicit def monoidInstance[E: Semigroup, A] = new Monoid[Validator[E, A]] {
    def empty = Validator.id[E, A]
    def combine(x: Validator[E, A], y: Validator[E, A]) = x && y
  }

  implicit def cartesianInstance[E: Semigroup] =
    new Cartesian[Validator[E, ?]] {
      override def product[A, B](fa: Validator[E, A],
                                 fb: Validator[E, B]): Validator[E, (A, B)] = {
        fa.product(fb)
      }
    }

  implicit def invariantInstance[E] = new Invariant[Validator[E, ?]] {
    override def imap[A, B](fa: Validator[E, A])
                           (f: (A) => B)
                           (g: (B) => A): Validator[E, B] = {
      fa.imap(f)(g)
    }
  }

  implicit def instanceForTraverse[E: Semigroup, F[_]: Traverse, A: Validator[E, ?]] =
    new Validator[E, F[A]] {
      def apply(subject: F[A]): Validated[E, F[A]] = {
        Traverse[F].sequence[Validated[E, ?], A] {
          Traverse[F].map(subject)(Validator[E, A])
        }
      }
    }

  implicit def instanceForTraverse2[E: Semigroup, F[_, _], A, B: Validator[E, ?]](implicit T: Traverse[F[A, ?]]) =
    new Validator[E, F[A, B]] {
      def apply(subject: F[A, B]): Validated[E, F[A, B]] = {
        Traverse[F[A, ?]].sequence[Validated[E, ?], B] {
          Traverse[F[A, ?]].map(subject)(Validator[E, B])
        }
      }
    }
}
