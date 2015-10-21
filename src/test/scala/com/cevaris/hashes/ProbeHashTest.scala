package com.cevaris.hashes

import org.scalatest.BeforeAndAfterEach


class ProbeHashTest
  extends HashesSpec
  with BeforeAndAfterEach {

  Seq(
    new LinearProbeHash[String](),
    new NStepProbeHash[String](n = 3),
    new QuadraticProbeHash[String](),
    new DoubleHashProbeHash[String]()
  ).foreach(testHash)

  def testHash(hashInstance: ProbeHash[String]) = {

    def newInstance(hashInstance: ProbeHash[String]): ProbeHash[String] = {
      hashInstance.clear()
      hashInstance
    }

    s"${hashInstance.getClass.getCanonicalName}" when {

      "handle empty hash" in {
        val actual = newInstance(hashInstance)
        actual.size() mustBe 0
      }

      "set" should {
        "handle hash single item" in {
          val actual = newInstance(hashInstance)
          actual.set(1, "test")
          actual.size() mustBe 1
        }

        "handle index single collision" in {
          val actual = newInstance(hashInstance)
          val ds = actual.defaultSize
          actual.set(1, "test1")
          actual.set(ds + 1, "test1b")
          actual.size() mustBe 2
        }

        "handle multiple set with same key items" in {
          val actual = newInstance(hashInstance)
          val ds = actual.defaultSize
          actual.set(1, "test1")
          actual.set(1, "test1a")
          actual.size() mustBe 1

          actual.get(1) mustBe Some("test1a")
        }

        "handle worst case scenario" in {
          val actual = newInstance(hashInstance)
          val ds = actual.defaultSize
          val numOfCollisions = 6
          (0 until ds * numOfCollisions by ds) foreach { i =>
            actual.set(i, s"test$i")
          }

          actual.size() mustBe numOfCollisions
        }
      }

      "get" should {
        "handle single item" in {
          val actual = newInstance(hashInstance)
          actual.set(100, "test")
          actual.size() mustBe 1

          actual.get(100) mustBe Some("test")
        }

        "handle item not found with multiple items" in {
          val actual = newInstance(hashInstance)
          val ds = actual.defaultSize
          actual.set(5, "test5")
          actual.set(ds + 5, "test5+")
          actual.size() mustBe 2

          actual.get(5) mustBe Some("test5")
          actual.get(ds + 5) mustBe Some("test5+")
          actual.get(ds * 2 + 5) mustBe None
        }

        "handle multiple non-colliding items" in {
          val actual = newInstance(hashInstance)
          val ds = actual.defaultSize
          actual.set(5, "test5")
          actual.set(ds, "test5+")
          actual.size() mustBe 2

          actual.get(5) mustBe Some("test5")
          actual.get(ds) mustBe Some("test5+")
        }

        "handle multiple colliding items" in {
          val actual = newInstance(hashInstance)
          val ds = actual.defaultSize
          actual.set(1, "test1")
          actual.set(ds + 1, "test1b")
          actual.size() mustBe 2

          actual.get(1) mustBe Some("test1")
          actual.get(ds + 1) mustBe Some("test1b")
        }

        "handle worst case scenario" in {
          val actual = newInstance(hashInstance)
          val ds = actual.defaultSize
          val numOfCollisions = 6
          (0 until ds * numOfCollisions by ds) foreach { i =>
            actual.set(i, s"test$i")
          }

          actual.size() mustBe numOfCollisions

          (0 until ds * numOfCollisions by ds) foreach { i =>
            actual.get(i) mustBe Some(s"test$i")
          }
        }
      }

      "remove" should {
        "handle single item" in {
          val actual = newInstance(hashInstance)
          actual.set(100, "test")

          actual.size() mustBe 1
          actual.remove(100) mustBe Some("test")
          actual.get(100) mustBe None
        }

        "handle multiple non-colliding items" in {
          val actual = newInstance(hashInstance)
          val ds = actual.defaultSize
          actual.set(5, "test5")
          actual.set(ds, "test5+")
          actual.size() mustBe 2

          actual.remove(5) mustBe Some("test5")
          actual.remove(5) mustBe None
          actual.size() mustBe 1
        }

        "handle multiple colliding items" in {
          val actual = newInstance(hashInstance)
          val ds = actual.defaultSize
          actual.set(1, "test1")
          actual.set(ds + 1, "test1b")
          actual.size() mustBe 2

          actual.remove(ds + 1) mustBe Some("test1b")
          actual.remove(ds + 1) mustBe None
          actual.size() mustBe 1
        }

      }

    }

  }
}
