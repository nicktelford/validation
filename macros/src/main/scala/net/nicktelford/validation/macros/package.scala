package net.nicktelford.validation

import cats.data.ValidatedNel

import scala.reflect.macros.blackbox

package object macros {

  def constraintImpl(c: blackbox.Context)
                    (cond: c.Tree): c.Expr[Unit] = {
    import c.universe._
    c.Expr(q"constraint($cond, ${show(cond)})")
  }

  // note: this macro currently doesn't work
  // todo: find a way to correctly inject the ValidationContext
  def validatedImpl[E, A](c: blackbox.Context)
                      (f: c.Tree): c.Expr[ValidatedNel[E, A]] = {
    import c.universe._

    c.Expr[ValidatedNel[E, A]](
      q"""
          implicit val ctx: ValidationContext.Validated =
            ValidationContext.validated

          ctx.validate($f)
      """)
  }
}
