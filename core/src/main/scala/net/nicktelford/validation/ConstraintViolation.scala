package net.nicktelford.validation

object ConstraintViolation {

  def apply(location: String, cause: String): ConstraintViolation =
    ConstraintViolation(location :: Nil, cause)
}

case class ConstraintViolation(path: List[String], cause: String) {
  val message: String = s"${path.mkString(".")} $cause"

  override def toString: String = message
}
