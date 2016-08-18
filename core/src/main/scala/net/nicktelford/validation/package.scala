import net.nicktelford.validation.{BasicConstraint, Constraint, NestedConstraint, Validator}

package object validation {

  def require[E, A](predicate: A => Boolean, error: E): Constraint[E, A] =
    BasicConstraint(predicate, _ => error)

  def validate[E, A, B](selector: A => B)
                       (implicit Validator: Validator[E, B]): Constraint[E, A] =
    NestedConstraint(selector)(Validator)

  def constraint[E, A](predicate: A => Boolean,
                       error: A => E): Constraint[E, A] =
    BasicConstraint(predicate, error)

  def constraint[E, A, B](selector: A => B)
                         (implicit Validator: Validator[E, B]): Constraint[E, A] =
    NestedConstraint(selector)(Validator)
}
