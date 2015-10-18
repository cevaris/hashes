package com.cevaris.hashes

class LinearProbeHashTest extends HashesSpec {

  "LinearProbeHash" when {

    "handle empty hash" in {
      val actual = new LinearProbeHash[String]()
      actual.size() mustBe 0
    }

    "set" should {
      "handle hash single item" in {
        val actual = new LinearProbeHash[String]()
        actual.set(1, "test")
        actual.size() mustBe 1
      }

      "handle index single collision" in {
        val actual = new LinearProbeHash[String]()
        val ds = actual.defaultSize
        actual.set(1, "test1")
        actual.set(ds + 1, "test1b")
        actual.size() mustBe 2
      }

      "handle worst case scenario" in {
        val actual = new LinearProbeHash[String]()
        val ds = actual.defaultSize
        val numberOfCollisions = 6
        (0 until ds * numberOfCollisions by ds) foreach { i =>
          actual.set(i, s"test$i")
        }

        actual.size() mustBe numberOfCollisions
      }
    }

    "get" should {
      "handle single item" in {
        val actual = new LinearProbeHash[String]()
        actual.set(100, "test")
        actual.size() mustBe 1

        actual.get(100) mustBe Some("test")
      }

      "handle multiple non-colliding items" in {
        val actual = new LinearProbeHash[String]()
        val ds = actual.defaultSize
        actual.set(5, "test5")
        actual.set(ds + 5, "test5+")
        actual.size() mustBe 2

        actual.get(5) mustBe Some("test5")
        actual.get(ds + 5) mustBe Some("test5+")
      }

      "handle multiple colliding items" in {
        val actual = new LinearProbeHash[String]()
        val ds = actual.defaultSize
        actual.set(1, "test1")
        actual.set(ds + 1, "test1b")
        actual.size() mustBe 2

        actual.get(1) mustBe Some("test1")
        actual.get(ds + 1) mustBe Some("test1b")
      }
    }


  }

}
