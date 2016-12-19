package net.nicktelford.validation

import cats.Show

object ConstraintViolation {

  def apply[A](subject: A, path: List[String], cause: String)
              (implicit A: Show[A] = defaultShow[A]): ConstraintViolation =
    ConstraintViolation(path, cause, Option(A.show(subject)).filter(_.nonEmpty))

  private def defaultShow[T]: Show[T] = (x: T) => ""
}

case class ConstraintViolation(path: List[String],
                               cause: String,
                               subject: Option[String]) {

  def message: String =
    (subject, path) match {
      case (None, Nil) => cause
      case (Some(subject), Nil) => s"$subject $cause"
      case (None, path) => s"${path.mkString(".")} $cause"
      case (Some(subject), path) => s"${path.mkString(".")} $cause, in: '$subject'"
    }
}
