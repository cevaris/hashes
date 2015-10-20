package com.cevaris.hashes


object Hashed {

  implicit class Sieve(val N: Int) extends AnyVal {
    def primes: Seq[Int] = {
      val isPrime = collection.mutable.BitSet(2 to N: _*) -- (4 to N by 2)
      for (p <- 2 +: (3 to Math.sqrt(N).toInt by 2) if isPrime(p)) {
        isPrime --= p * p to N by p
      }
      isPrime.toImmutable.toSeq
    }
  }

}

trait Hashed[A] {

  def get(key: Int): Option[A]

  /**
   * @return inserted value A
   */
  def set(key: Int, value: A): A

  def size(): Int

  /**
   * Number of elements cleared
   * @return
   */
  def clear(): Int

  /**
   * @return Option[A] Item removed if found
   */
  def remove(key: Int): Option[A]
}


case class KeyValue[A](key: Int, value: A)