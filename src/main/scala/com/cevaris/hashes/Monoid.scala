package com.cevaris.hashes

trait Monoid[T] {
  def zero: T
}

object Monoid {

  implicit object StringMonoid extends Monoid[String] {
    def zero: String = ""
  }

}