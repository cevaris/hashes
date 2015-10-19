package com.cevaris.hashes

import com.cevaris.hashes.Hashed.Sieve
import com.twitter.logging.Logger
import scala.reflect.ClassTag


abstract class ProbeHash[A](implicit m: ClassTag[A]) extends Hashed[A] {

  private val log = Logger.get(getClass)

  val defaultSize: Int = 19
  var table: Array[KeyValue[A]] = empty()

  protected def collisionFunction(attempt: Int): Int =
    attempt + 1

  protected def hashFunction(key: Int): Int = key

  override def get(key: Int): Option[A] =
    getLinearProbe(key, 0)

  override def set(key: Int, value: A): A = {
    table = setLinearProbe(key, 0, value)
    value
  }

  override def clear(): Int = {
    val currSize = size()
    table = empty()
    currSize
  }

  override def size(): Int =
    table.count(_ != null)

  protected def empty(): Array[KeyValue[A]] =
    new Array(defaultSize)

  protected def hash(key: Int, attempt: Int, currSize: Int) =
    (hashFunction(key) + collisionFunction(attempt)) % currSize

  protected def getLinearProbe(key: Int, attempt: Int): Option[A] = {
    val currentIndex = hash(key, attempt, defaultSize)
    table.lift(currentIndex) match {
      case None | Some(null) => None
      case Some(currKeyValue) =>
        if (currKeyValue.key == key) {
          Some(currKeyValue.value)
        } else {
          log.warning(s"Collision with $currKeyValue at $currentIndex when looking for $key")
          getLinearProbe(key, attempt + 1)
        }
    }
  }

  protected def setLinearProbe(key: Int, attempt: Int, value: A): Array[KeyValue[A]] = {
    val currentIndex = hash(key, attempt, defaultSize)
    table.lift(currentIndex) match {
      case None | Some(null) =>
        table.updated(currentIndex, KeyValue(key, value))
      case Some(currKeyValue) =>
        log.warning(s"Collision with $currKeyValue at $currentIndex")
        setLinearProbe(key, attempt + 1, value)
    }
  }

}


class LinearProbeHash[A](implicit m: ClassTag[A]) extends ProbeHash[A] {
  override protected def collisionFunction(attempt: Int): Int =
    attempt + 1

}


class NStepProbeHash[A](n: Int)(implicit m: ClassTag[A]) extends ProbeHash[A] {
  override protected def collisionFunction(attempt: Int): Int =
    attempt + n

}


class QuadraticProbeHash[A](implicit m: ClassTag[A]) extends ProbeHash[A] {
  override protected def collisionFunction(attempt: Int): Int =
    Math.pow(attempt, 2).toInt

}


class DoubleHashProbeHash[A](implicit m: ClassTag[A]) extends ProbeHash[A] {
  import scala.util.control.Breaks._

  // http://www.cs.yorku.ca/~billk/Teaching/lectureNotesHashTables.pdf
  // The offset is static

  private val log = Logger.get(getClass)

  private val prevPrime = table.length.primes.reverse.head

  private def hash2(key: Int, attempt: Int, currSize: Int) = {
    val prevHash = hash(key, attempt, currSize)
    prevPrime - (prevHash % prevPrime)
  }

  override def getLinearProbe(key: Int, attempt: Int): Option[A] = {
    var currentIndex = hash(key, attempt, defaultSize)
    val offsetIndex = hash2(key, attempt, defaultSize)

    if (table.lift(currentIndex).exists(_.key == key)) {
      return table.lift(currentIndex).map(kv => kv.value)
    }

    breakable {
      while (!table.lift(currentIndex).contains(null)) {
        if (table.lift(currentIndex).exists(_.key != key)) {
          // Collision
          currentIndex = (currentIndex + offsetIndex) % table.length
        } else {
          break()
        }
      }
    }

    table.lift(currentIndex).map(kv => kv.value)

  }

  override def setLinearProbe(key: Int, attempt: Int, value: A): Array[KeyValue[A]] = {
    var currentIndex: Int = hash(key, attempt, defaultSize)
    val offsetIndex: Int = hash2(key, attempt, defaultSize)

    while (!table.lift(currentIndex).contains(null))
      currentIndex = (currentIndex + offsetIndex) % table.length

    table.updated(currentIndex, KeyValue(key, value))
  }

}

