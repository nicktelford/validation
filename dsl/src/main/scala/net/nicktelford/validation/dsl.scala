package net.nicktelford.validation

import cats.data.Validated.valid
import cats.data.Validated
import cats.{Eq, Monoid}

import scala.language.experimental.{macros => enableMacros}

object dsl
  extends ValidatorConstructors
    with MonoidCombinators
    with NumericCombinators
    with EitherCombinators

trait ValidatorConstructors {

  class Require[A] {

    def apply[B](selector: A => B,
                 predicate: B => Boolean,
                 msg: String): ConstraintValidator[A] =
      macro macros.validatePredicate[A, B]

    def apply[B](selector: A => B, validator: ConstraintValidator[B]): ConstraintValidator[A] =
      macro macros.validate[A, B]

    def apply[B](selector: A => B,
                 path: List[String],
                 predicate: B => Boolean,
                 msg: String): ConstraintValidator[A] =
      apply[B](selector, path, ConstraintValidator[B](predicate, msg))

    def apply[B](selector: A => B,
                 path: List[String],
                 validator: ConstraintValidator[B]): ConstraintValidator[A] =
      ConstraintValidator(selector, path, validator)
  }

  class Validate[A] {

    def apply[B](selector: A => B)
                (implicit validator: ConstraintValidator[B]): ConstraintValidator[A] =
      macro macros.validateImplicit[A, B]


    def apply[B](selector: A => B, path: List[String])
                (implicit validator: ConstraintValidator[B]): ConstraintValidator[A] =
      ConstraintValidator(selector, path, validator)
  }

  def require[A]: Require[A] = new Require[A]

  def validate[A]: Validate[A] = new Validate[A]
}

trait MonoidCombinators { self: ValidatorConstructors =>

  def isEmpty[M: Eq](implicit M: Monoid[M]) =
    ConstraintValidator((x: M) => M.isEmpty(x), "must be empty")

  def notEmpty[M: Eq](implicit M: Monoid[M]) =
    ConstraintValidator((x: M) => !M.isEmpty(x), "must not be empty")
}

trait CollectionCombinators { self: ValidatorConstructors =>
  private type Collection = collection.GenTraversableOnce[_]

  val isEmpty =
    Validator((x: Collection) => x.isEmpty, "must be empty")

  val notEmpty =
    Validator((x: Collection) => x.nonEmpty, "must not be empty")
}

trait NumericCombinators { self: ValidatorConstructors =>
  // todo: specialize for numeric values

  def isZero[N](implicit N: Numeric[N]) =
    ConstraintValidator(N.zero == _, "must be zero")

  def nonZero[N](implicit N: Numeric[N]) =
    ConstraintValidator(N.zero != _, "must not be zero")

  def isPositive[N](implicit N: Numeric[N]) =
    ConstraintValidator((x: N) => N.gt(x, N.zero), "must be positive")

  def isNegative[N](implicit N: Numeric[N]) =
    ConstraintValidator((x: N) => N.lt(x, N.zero), "must be negative")
}

trait EitherCombinators { self: ValidatorConstructors =>

  def whenRight[E, L, R](validator: Validator[E, R]) =
    new Validator[E, Either[_, R]] {
      override def apply(v1: Either[_, R]): Validated[E, Either[_, R]] = {
        v1 match {
          case Left(_) => valid(v1)
          case Right(r) => validator(r).map(Right(_))
        }
      }
    }

  def whenLeft[E, L](validator: Validator[E, L]) =
    new Validator[E, Either[L, _]] {
      override def apply(v1: Either[L, _]): Validated[E, Either[L, _]] = {
        v1 match {
          case Right(_) => valid(v1)
          case Left(l) => validator(l).map(Left(_))
        }
      }
    }
}
