package net.nicktelford.validation

case class ConstraintViolation(msg: String) extends Exception(msg)

