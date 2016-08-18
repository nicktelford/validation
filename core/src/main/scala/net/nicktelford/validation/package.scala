import net.nicktelford.validation.{Constraint, Validator}

package object validation {
//
//  def constraint[E, A](predicate: A => Boolean, error: E): Constraint[E, A] =
//    Constraint(predicate, _ => error)

  def constraint[E, A](predicate: A => Boolean,
                       error: A => E): Constraint[E, A] =
    Constraint(predicate, error)

//  def constraint[E, A, B](selector: A => B)
//                         (implicit Validator: Validator[E, B]): Constraint[E, A] = {
//    Constraint(Validator.validate(selector))
//  }
}
