package net.nicktelford.validation

import cats.implicits._
import cats.data.{ValidatedNel, NonEmptyList => NEL}
import cats.data.Validated.{invalidNel, valid}

object Validator {

  def apply[A](constraints: Validator[ConstraintViolation, A]*): Validator[ConstraintViolation, A] =
    new MultipleValidator[ConstraintViolation, A](constraints.toList)

  def custom[E, A](constraints: Validator[E, A]*): Validator[E, A] =
    new MultipleValidator[E, A](constraints.toList)
}

trait Validator[E, A] {
  def validate(subject: A): ValidatedNel[E, A]
  def leftMap[EE](f: E => EE): Validator[EE, A] = new Validator[EE, A] {
    override def validate(subject: A): ValidatedNel[EE, A] =
      Validator.this.validate(subject).leftMap(_.map(f))
  }
}

class ConstraintValidator[E, A](predicate: A => Boolean, error: A => E)
  extends Validator[E, A] {

  override def validate(subject: A): ValidatedNel[E, A] =
    if (predicate(subject)) valid(subject)
    else invalidNel(error(subject))
}

class NestedValidator[E, A, B](selector: A => B)(implicit V: Validator[E, B])
  extends Validator[E, A] {

  override def validate(subject: A): ValidatedNel[E, A] = {
    V.validate(selector(subject)).map(_ => subject)
  }
}

class MultipleValidator[E, A](validators: List[Validator[E, A]])
  extends Validator[E, A] {

  override def validate(subject: A): ValidatedNel[E, A] = {
    validators.map(_.validate(subject)).foldLeft(valid[NEL[E], A](subject)) {
      // this seems like abuse of Apply#ap, better way to do it?
      // the main problem of this is that we ignore previous successful values
      // but since they're guaranteed to be the same, that's ok here.
      (acc, x) => x.ap(acc.map(y => _ => y))
    }
  }
}
