package com.cevaris.hashes

import com.cevaris.hashes.Hashed.Sieve
import com.twitter.logging.Logger


abstract class ProbeHash[A] extends Hashed[A] {

  private val log = Logger.get(getClass)

  val defaultSize: Int = 19
  var table: Array[Option[KeyValue[A]]] = empty()

  protected def collisionFunction(attempt: Int): Int =
    attempt + 1

  protected def hashFunction(key: Int): Int = key

  override def get(key: Int): Option[A] =
    getLinearProbe(key, 0, Get)

  override def set(key: Int, value: A): A = {
    table = setLinearProbe(key, 0, value)
    value
  }

  override def remove(key: Int): Option[A] =
    getLinearProbe(key, 0, Remove)

  override def clear(): Int = {
    val currSize = size()
    table = empty()
    currSize
  }

  override def size(): Int =
    table.flatten.length

  protected def empty(): Array[Option[KeyValue[A]]] =
    Array.fill(defaultSize)(None)

  protected def hash(key: Int, attempt: Int, currSize: Int) =
    (hashFunction(key) + collisionFunction(attempt)) % currSize

  protected def getLinearProbe(key: Int, attempt: Int, hashOperation: HashOperation): Option[A] = {
    val currentIndex = hash(key, attempt, defaultSize)
    table(currentIndex) match {
      case None => None
      case Some(currKeyValue) =>
        if (currKeyValue.key == key) {
          val result = Some(currKeyValue.value)
          if (hashOperation == Remove) {
            table.update(currentIndex, None)
          }
          result
        } else {
          log.warning(s"Collision with $currKeyValue at $currentIndex when looking for $key")
          getLinearProbe(key, attempt + 1, hashOperation)
        }
    }
  }

  protected def setLinearProbe(key: Int, attempt: Int, value: A): Array[Option[KeyValue[A]]] = {
    val currentIndex = hash(key, attempt, defaultSize)
    table(currentIndex) match {
      case None =>
        table.updated(currentIndex, Some(KeyValue(key, value)))
      case Some(currKeyValue) =>
        log.warning(s"Collision with $currKeyValue at $currentIndex")
        setLinearProbe(key, attempt + 1, value)
    }
  }

}


class LinearProbeHash[A] extends ProbeHash[A] {
  override protected def collisionFunction(attempt: Int): Int =
    attempt + 1

}


class NStepProbeHash[A](n: Int) extends ProbeHash[A] {
  override protected def collisionFunction(attempt: Int): Int =
    attempt + n

}


class QuadraticProbeHash[A] extends ProbeHash[A] {
  override protected def collisionFunction(attempt: Int): Int =
    Math.pow(attempt, 2).toInt

}


class DoubleHashProbeHash[A] extends ProbeHash[A] {

  import scala.util.control.Breaks._

  private val log = Logger.get(getClass)

  private val prevPrime = table.length.primes.reverse.head

  private def hash2(key: Int, attempt: Int, currSize: Int) = {
    val prevHash = hash(key, attempt, currSize)
    prevPrime - (prevHash % prevPrime)
  }

  override def getLinearProbe(key: Int, attempt: Int, hashOperation: HashOperation): Option[A] = {
    var currentIndex = hash(key, attempt, defaultSize)
    val offsetIndex = hash2(key, attempt, defaultSize)

    if (table(currentIndex).exists(_.key == key)) {

      val result = table(currentIndex).map(kv => kv.value)
      if (hashOperation == Remove) {
        table.update(currentIndex, None)
      }

      return result
    }

    breakable {
      while (table(currentIndex).isDefined) {
        if (table(currentIndex).exists(_.key != key)) {
          // Collision
          currentIndex = (currentIndex + offsetIndex) % table.length
        } else {
          break()
        }
      }
    }

    if (table(currentIndex).isDefined) {
      val result = table(currentIndex).map(kv => kv.value)
      if (hashOperation == Remove) {
        table.update(currentIndex, None)
      }
      result
    } else {
      None
    }


  }

  override def setLinearProbe(key: Int, attempt: Int, value: A): Array[Option[KeyValue[A]]] = {
    var currentIndex: Int = hash(key, attempt, defaultSize)
    val offsetIndex: Int = hash2(key, attempt, defaultSize)

    while (!table.lift(currentIndex).contains(None))
      currentIndex = (currentIndex + offsetIndex) % table.length

    table.updated(currentIndex, Some(KeyValue(key, value)))
  }

}

