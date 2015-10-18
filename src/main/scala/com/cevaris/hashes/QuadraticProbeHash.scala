package com.cevaris.hashes

import com.twitter.logging.Logger
import scala.reflect.ClassTag

class QuadraticProbeHash[A](implicit m: ClassTag[A]) extends Hashed[A] {

  private val log = Logger.get(getClass)

  val defaultSize: Int = 19
  var table: Array[KeyValue[A]] = empty()

  override def get(key: Int): Option[A] =
    getLinearProbe(key, key % defaultSize)

  override def set(key: Int, value: A): A = {
    val initialIndex: Int = key % defaultSize
    table = setLinearProbe(key, initialIndex, value)
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

  private def getLinearProbe(key: Int, currentIndex: Int): Option[A] = {
    table.lift(currentIndex) match {
      case None | Some(null) => None
      case Some(currKeyValue) =>
        if (currKeyValue.key == key) {
          Some(currKeyValue.value)
        } else {
          log.warning(s"Collision with $currKeyValue at $currentIndex when looking for $key")
          getLinearProbe(key, currentIndex + 1)
        }
    }

  }

  private def setLinearProbe(key: Int, currentIndex: Int, value: A): Array[KeyValue[A]] = {
    table.lift(currentIndex) match {
      case None | Some(null) =>
        table.updated(currentIndex, KeyValue(key, value))
      case Some(currKeyValue) =>
        log.warning(s"Collision with $currKeyValue at $currentIndex")
        setLinearProbe(key, currentIndex + 1, value)
    }
  }

}