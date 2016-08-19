import net.nicktelford.validation._

package object validation {

  def require[A](predicate: A => Boolean,
                 error: => String): Validator[ConstraintViolation, A] =
    new ConstraintValidator(predicate, _ => ConstraintViolation(error))

  def constraint[E, A](predicate: A => Boolean, error: A => E): Validator[E, A] =
    new ConstraintValidator(predicate, error)

  def constraint[E, A, B](selector: A => B)
                         (implicit Validator: Validator[E, B]): Validator[E, A] =
    new NestedValidator(selector)(Validator)

  def validate[E, A, B](selector: A => B)
                       (implicit V: Validator[E, B]): Validator[E, A] =
    new NestedValidator(selector)(V)
}
