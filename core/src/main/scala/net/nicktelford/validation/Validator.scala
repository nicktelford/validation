package net.nicktelford.validation

import cats.Show
import cats.implicits._
import cats.data.{Validated, ValidatedNel, NonEmptyList => NEL}
import cats.data.Validated.{invalidNel, valid}

object Validator {

  def apply[E: Show, A](constraints: Constraint[E, A]*): Validator[E, A] =
    ConstraintValidator[E, A](constraints.toList)
}

trait Validator[E, A] {

  private[validation] def validateConstraints(subject: A): ValidatedNel[E, A]

  def validate(subject: A): Validated[ConstraintViolations[E, A], A]

  def leftMap[EE: Show](f: E => EE): Validator[EE, A] = {
    new Validator[EE, A] {
      override private[validation] def validateConstraints(subject: A): ValidatedNel[EE, A] = {
        Validator.this.validateConstraints(subject)
          .leftMap(_.map(f))
      }
      override def validate(subject: A): Validated[ConstraintViolations[EE, A], A] = {
        Validator.this.validate(subject)
          .leftMap(x => x.copy(reasons = x.reasons.map(f)))
      }
    }
  }
}

sealed trait Constraint[E, A] {
  def validate(subject: A): ValidatedNel[E, A]
}

case class BasicConstraint[E, A](predicate: A => Boolean, error: A => E) extends Constraint[E, A] {
  override def validate(subject: A): ValidatedNel[E, A] = {
    if (predicate(subject)) valid[NEL[E], A](subject)
    else invalidNel[E, A](error(subject))
  }
}

case class NestedConstraint[E, A, B](selector: A => B)
                                    (implicit Validator: Validator[E, B]) extends Constraint[E, A] {
  override def validate(subject: A): ValidatedNel[E, A] =
  // todo: instead of overriding the subject, we should override it in the result (right-side) and instead construct a Path the correct subject in the ConstraintViolations
    Validator.validateConstraints(selector(subject)).map(_ => subject)
}

case class ConstraintValidator[E: Show, A](constraints: List[Constraint[E, A]])
  extends Validator[E, A] {

  override private[validation] def validateConstraints(subject: A): ValidatedNel[E, A] = {
    constraints.map(_.validate(subject)).foldLeft(valid[NEL[E], A](subject)) {
      // this seems like abuse of Apply#ap, better way to do it?
      // the main problem of this is that we ignore previous successful values
      // but since they're guaranteed to be the same, that's ok here.
      (acc, x) => x.ap(acc.map(y => (_: A) => y))
    }
  }

  def validate(subject: A): Validated[ConstraintViolations[E, A], A] = {
    validateConstraints(subject).leftMap(ConstraintViolations(subject, _))
  }
}

