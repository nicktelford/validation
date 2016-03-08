package net.nicktelford.validation

import cats.data.{NonEmptyList, ValidatedNel}
import cats.data.Validated.{valid, invalid}

trait ValidationContext {

  def constraint(cond: Boolean, msg: => String): Unit
}

object ValidationContext {

  def require = new ValidationContext {
    override def constraint(cond: Boolean, msg: => String): Unit =
      if (!cond)
        throw new IllegalArgumentException(s"constraint failed: $msg")
  }

  def validated = new Validated

  class Validated private[validation] extends ValidationContext {

    var violations: List[ConstraintViolation] = Nil

    override def constraint(cond: Boolean, msg: => String): Unit = {
      if (!cond)
        violations = ConstraintViolation(msg) :: violations
    }

    def validate[A](f: A): ValidatedNel[ConstraintViolation, A] = {
      NonEmptyList.fromList(violations.reverse) match {
        case None      => valid(f)
        case Some(err) => invalid(err)
      }
    }
  }
}
