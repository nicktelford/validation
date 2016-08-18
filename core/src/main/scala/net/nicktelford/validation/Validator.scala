package net.nicktelford.validation

import cats.Show
import cats.implicits._
import cats.data.{Validated, NonEmptyList => NEL}
import cats.data.Validated.{invalidNel, valid}

object Validator {

  def apply[E: Show, A](constraints: Constraint[E, A]*): Validator[E, A] =
    ConstraintValidator[E, A](constraints.toList)
}

trait Validator[E, A] {

  def validate(subject: A): Validated[ConstraintViolations[E, A], A]

  def leftMap[EE: Show](f: E => EE): Validator[EE, A] = {
    new Validator[EE, A] {
      override def validate(subject: A): Validated[ConstraintViolations[EE, A], A] = {
        Validator.this.validate(subject)
          .leftMap(x => x.copy(reasons = x.reasons.map(f)))
      }
    }
  }
}

case class Constraint[E, A](predicate: A => Boolean, error: A => E)

case class ConstraintValidator[E: Show, A](constraints: List[Constraint[E, A]])
  extends Validator[E, A] {

  def validate(subject: A): Validated[ConstraintViolations[E, A], A] = {
    constraints
      // validate constraints
      .map {
        case Constraint(predicate, _) if predicate(subject) =>
          valid[NEL[E], A](subject)
        case Constraint(_, error) =>
          invalidNel[E, A](error(subject))
      }
      // merge our results down to a NEL of failures or the successful result
      .foldLeft(valid[NEL[E], A](subject)) {
        // this seems like abuse of Apply#ap, better way to do it?
        // the main problem of this is that we ignore previous successful values
        // but since they're guaranteed to be the same, that's ok here.
        (acc, x) => x.ap(acc.map(y => (_: A) => y))
      }
      // lift failures in to a ConstraintViolations, which includes the subject
      .leftMap(ConstraintViolations(subject, _))
  }
}

