package com.codahale.bert

object Tuple {
  def apply[A](xs: A*): Tuple[A] = new Tuple(xs.toList)
  def unapplySeq[A](x: Tuple[A]): Some[Tuple[A]] = Some(x)
}

class Tuple[+A](private val items: List[A]) extends Seq[A] {
  def length = items.length
  def elements = items.elements
  def apply(n: Int) = items(n)
  override def toString = "Tuple(" + items.mkString(",") + ")"
}
