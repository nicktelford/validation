package net.nicktelford.validation

case class ConstraintViolation(cause: String)
  extends Exception(s"Constraint violation: $cause")

