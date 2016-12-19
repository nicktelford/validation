package net.nicktelford

import cats.data.{NonEmptyList => NEL}
import cats.data.ValidatedNel

package object validation {

  type ConstraintValidator[A] = Validator[NEL[ConstraintViolation], A]

  implicit class ValidatorOps[A](val subject: A) extends AnyVal {
    def validated(implicit validator: ConstraintValidator[A]): ValidatedNel[ConstraintViolation, A] =
      validator(subject)
  }

  implicit object instances extends ValidatorInstances
}
