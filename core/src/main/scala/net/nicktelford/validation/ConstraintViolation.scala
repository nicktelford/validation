package net.nicktelford.validation

import cats.Show

object ConstraintViolation {

  object Implicits extends Implicits
  trait Implicits {
    implicit object RootNodeShow extends Show[Root.type] {
      override def show(f: Root.type): String = ""
    }

    implicit def ObjectNodeShow(implicit S: Show[Node]) = new Show[/] {
      override def show(f: /): String = S.show(f.parent) match {
        case "" => f.name
        case p => s"$p.${f.name}"
      }
    }

    implicit def ArrayNodeShow(implicit S: Show[Node]) = new Show[/#] {
      override def show(f: /#): String = S.show(f.parent) match {
        case "" => s"[${f.idx}]"
        case p => s"$p[${f.idx}]"
      }
    }

    implicit object NodeShow extends Show[Node] {
      override def show(f: Node): String = f match {
        case Root => RootNodeShow.show(Root)
        case x: / => ObjectNodeShow(this).show(x)
        case x: /# => ArrayNodeShow(this).show(x)
      }
    }
  }

  def apply(location: String, cause: String)
           (implicit S: Show[Node]): ConstraintViolation =
    ConstraintViolation(Root / location, cause)
}

case class ConstraintViolation(path: Node, cause: String)
                              (implicit S: Show[Node]){
  val message: String = s"${S.show(path)} $cause"

  override def toString: String = message
}

sealed trait Node {
  def /(name: String) = new /(name, this)
  def /#(idx: Int) = new /#(idx, this)
  def parent(parent: Node): Node
}

case object Root extends Node {
  override def parent(parent: Node): Node = parent
}

case class /(name: String, parent: Node) extends Node {
  override def parent(parent: Node): Node = copy(parent = parent)
}
case class /#(idx: Int, parent: Node) extends Node {
  override def parent(parent: Node): Node = copy(parent = parent)
}
