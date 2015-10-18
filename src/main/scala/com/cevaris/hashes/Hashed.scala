package com.cevaris.hashes


trait Hashed[A] {

  def get(key: Int): Option[A]

  /**
   * @param key
   * @param value
   * @return inserted value A
   */
  def set(key: Int, value: A): A

  def size(): Int

  /**
   * Number of elements cleared
   * @return
   */
  def clear(): Int
}


case class KeyValue[A](key : Int, value: A)