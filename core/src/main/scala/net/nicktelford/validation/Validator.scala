package net.nicktelford.validation

import cats.Traverse
import cats.implicits._
import cats.data.{ValidatedNel, NonEmptyList => NEL}
import cats.data.Validated.{Invalid, Valid, invalidNel, valid}

object Validator {

  def apply[A](implicit V: Validator[ConstraintViolation, A]): Validator[ConstraintViolation, A] = V

  def of[A](constraints: Validator[ConstraintViolation, A]*): Validator[ConstraintViolation, A] =
    new MultipleValidator[ConstraintViolation, A](constraints.toList)

  def custom[E, A](constraints: Validator[E, A]*): Validator[E, A] =
    new MultipleValidator[E, A](constraints.toList)
}

trait Validator[E, A] {

  private[validation] val path: Node = Root
  private[validation] def parent(parents: Node): Validator[E, A] =
    new Validator[E, A] {
      override private[validation] val path: Node =
        Validator.this.path.parent(parents)

      override def validate(subject: A): ValidatedNel[E, A] =
        Validator.this.validate(subject)
    }

  def validate(subject: A): ValidatedNel[E, A]

  def mapErrors[EE](f: E => EE): Validator[EE, A] = new Validator[EE, A] {
    override def validate(subject: A): ValidatedNel[EE, A] =
      Validator.this.validate(subject).leftMap(_.map(f))
  }

  // from Invariant
  def imap[B](f: A => B)(g: B => A): Validator[E, B] = new Validator[E, B] {
    override def validate(subject: B): ValidatedNel[E, B] = {
      Validator.this.validate(g(subject)).map(f)
    }
  }

  // from Semigroup/Monoid
  def combine(other: Validator[E, A]): Validator[E, A] = new Validator[E, A] {
    override def validate(subject: A): ValidatedNel[E, A] = {
      (Validator.this.validate(subject), other.validate(subject)) match {
        case (x @ Valid(_), Valid(_)) => x
        case (Invalid(e1), Invalid(e2)) => Invalid(e1.combine(e2))
        case (x @ Invalid(_), _) => x
        case (_, x @ Invalid(_)) => x
      }
    }
  }

  // from Cartesian
  def product[B](other: Validator[E, B]): Validator[E, (A, B)] = new Validator[E, (A, B)] {
    override def validate(subject: (A, B)): ValidatedNel[E, (A, B)] = {
      Validator.this.validate(subject._1).product(other.validate(subject._2))
    }
  }
}

class ConstraintValidator[E, A](predicate: A => Boolean,
                                error: (A, Node) => E,
                                override val path: Node = Root)
  extends Validator[E, A] {

  override private[validation]
  def parent(parents: Node): Validator[E, A] =
    new ConstraintValidator[E, A](predicate, error, path.parent(parents))

  override def validate(subject: A): ValidatedNel[E, A] =
    if (predicate(subject)) valid(subject)
    else invalidNel(error(subject, path))
}

class NestedValidator[E, A, B](selector: A => B, override val path: Node = Root)
                              (implicit V: Validator[E, B])
  extends Validator[E, A] {

  override private[validation]
  def parent(parents: Node): Validator[E, A] =
    new NestedValidator[E, A, B](selector, path.parent(parents))(V)

  override def validate(subject: A): ValidatedNel[E, A] = {
    V.parent(path).validate(selector(subject)).map(_ => subject)
  }
}

class MultipleValidator[E, A](validators: List[Validator[E, A]],
                              override val path: Node = Root)
  extends Validator[E, A] {

  override private[validation]
  def parent(parents: Node): Validator[E, A] =
    new MultipleValidator[E, A](validators, path.parent(parents))

  override def validate(subject: A): ValidatedNel[E, A] = {
    validators
      .map(_.parent(path).validate(subject))
      .foldLeft(valid[NEL[E], A](subject)) {
        // this seems like abuse of Apply#ap, better way to do it?
        // the main problem of this is that we ignore previous successful values
        // but since they're guaranteed to be the same, that's ok here.
        (acc, x) => x.ap(acc.map(y => _ => y))
      }
  }
}
