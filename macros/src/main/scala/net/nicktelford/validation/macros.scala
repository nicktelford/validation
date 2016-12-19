package net.nicktelford.validation

import scala.annotation.tailrec
import scala.reflect.macros.blackbox

object macros {
  import scala.language.experimental.macros

  def determinePath[A, B](c: blackbox.Context)(selector: c.Expr[A => B]): c.Expr[List[String]] = {
    import c.universe._

    @tailrec
    def genTree(expr: Tree, parent: List[c.Expr[String]] = Nil): List[c.Expr[String]] = expr match {
      case Ident(_) => parent
      case q"$next.$name" => genTree(next, c.Expr(q"${name.decodedName.toString}") :: parent)
      case _ => throw new NotImplementedError(
        "Unable to extract a 'path' from this expression; explicitly provide a path for this selector as the second argument.")
    }

    val x = selector.tree match {
      case Function(_, x) => genTree(x)
      case _ =>  throw new NotImplementedError(
        "Unable to extract a 'path' from this expression; explicitly provide a path for this selector as the second argument.")
    }

    c.Expr(q"List(..$x)")
  }

  def validate[A: c.WeakTypeTag, B: c.WeakTypeTag]
              (c: blackbox.Context)
              (selector: c.Expr[A => B],
               validator: c.Expr[ConstraintValidator[B]]): c.Expr[ConstraintValidator[A]] = {
    import c.universe._

    val path = determinePath(c)(selector)

    c.Expr(q"ConstraintValidator($selector, $path, $validator)")
  }

  def validateImplicit[A: c.WeakTypeTag, B: c.WeakTypeTag]
                      (c: blackbox.Context)
                      (selector: c.Expr[A => B])
                      (validator: c.Expr[ConstraintValidator[B]]): c.Expr[ConstraintValidator[A]] =
    validate(c)(selector, validator)

  def validatePredicate[A: c.WeakTypeTag, B: c.WeakTypeTag]
                       (c: blackbox.Context)
                       (selector: c.Expr[A => B],
                        predicate: c.Expr[B => Boolean],
                        msg: c.Expr[String]): c.Expr[ConstraintValidator[A]] = {
    import c.universe._

    val path = determinePath(c)(selector)

    c.Expr(q"ConstraintValidator($selector, $path, ConstraintValidator($predicate, $msg))")
  }
}
