package com.cevaris.hashes


object HashExceptions {
  def throwExceededAllocatedSpaceException(msg: String) =
    throw ExceededAllocatedSpaceException(msg)

}

abstract class HashExceptions(msg: String) extends RuntimeException(msg)

case class ExceededAllocatedSpaceException(msg: String) extends HashExceptions(msg)



