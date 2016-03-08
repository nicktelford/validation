package net.nicktelford

import cats.data.ValidatedNel

import net.nicktelford.validation.macros._

package object validation {

  import scala.language.experimental.macros

  def validated[A](f: => A): ValidatedNel[ConstraintViolation, A] =
    macro validatedImpl[ConstraintViolation, A]

  def constraint(cond: Boolean): Unit = macro constraintImpl

  def constraint(cond: Boolean, msg: => String)
                (implicit v: ValidationContext): Unit =
    v.constraint(cond, msg)
}
