package com.cevaris.hashes

import com.twitter.logging.Logger
import scala.reflect.ClassTag

class QuadraticProbeHash[A](implicit m: ClassTag[A]) extends Hashed[A] {

  private val log = Logger.get(getClass)

  val defaultSize: Int = 19
  var table: Array[KeyValue[A]] = empty()

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

  private def empty(): Array[KeyValue[A]] =
    new Array(defaultSize)

  private def collisionFunction(attempt: Int): Int =
    attempt + 1

  private def hashFunction(key: Int): Int =
    key

  private def hash(key: Int, attempt: Int, currSize: Int) = {
    (hashFunction(key) + collisionFunction(attempt)) % currSize
  }


  private def getLinearProbe(key: Int, attempt: Int): Option[A] = {
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

  private def setLinearProbe(key: Int, attempt: Int, value: A): Array[KeyValue[A]] = {
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