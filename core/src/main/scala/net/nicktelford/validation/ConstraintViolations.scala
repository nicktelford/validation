package net.nicktelford.validation

import cats.implicits._
import cats.Show
import cats.data.{NonEmptyList => NEL}

case class ConstraintViolations[E, A](subject: A, reasons: NEL[E])
                                     (implicit Show: Show[E])
  extends Exception(
    s"Constraint violation on ${subject.getClass.getSimpleName}: ${reasons.foldLeft("")((acc, x) => acc + s", ${Show.show(x)}")}"
  )
