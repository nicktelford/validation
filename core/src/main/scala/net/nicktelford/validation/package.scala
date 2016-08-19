import cats.Cartesian
import cats.data.ValidatedNel
import cats.data.Validated.valid
import cats.functor.Invariant
import cats.kernel.Monoid
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

  implicit def validatorInstances1[E] =
    new Cartesian[Validator[E, ?]] with Invariant[Validator[E, ?]] {
      override def product[A, B](fa: Validator[E, A],
                                 fb: Validator[E, B]): Validator[E, (A, B)] =
        fa product fb

      override def imap[A, B](fa: Validator[E, A])
                             (f: (A) => B)
                             (g: (B) => A): Validator[E, B] = fa.imap(f)(g)
    }

  implicit def validatorInstances[E, A] =
    new Monoid[Validator[E, A]] {
      override def empty: Validator[E, A] = new Validator[E, A] {
        override def validate(subject: A): ValidatedNel[E, A] = valid(subject)
      }

      override def combine(x: Validator[E, A],
                           y: Validator[E, A]): Validator[E, A] = x combine y
    }
}
