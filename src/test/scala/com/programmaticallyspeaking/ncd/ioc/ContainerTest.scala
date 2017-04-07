package com.programmaticallyspeaking.ncd.ioc

import com.programmaticallyspeaking.ncd.testing.UnitTest

class ContainerTest extends UnitTest {
  import ContainerTest._

  "a container" - {
    "with the necessary references" - {
      val a = new A
      val b = new B
      val container = new Container(Seq(a, b))

      "can create an instance of a target that wants the references" in {
        val instance = container.newInstance(classOf[Target])
        instance.a shouldBe theSameInstanceAs (a)
      }

      "passes all references" in {
        val instance = container.newInstance(classOf[Target])
        instance.b shouldBe theSameInstanceAs (b)
      }

      "rejects creation of a target with no public constructor" in {
        assertThrows[IllegalArgumentException](container.newInstance(classOf[NoPublicConstructor]))
      }

      "rejects creation of a target with multiple public constructors" in {
        assertThrows[IllegalArgumentException](container.newInstance(classOf[MultiplePublicConstructors]))
      }

      "can create an instance based on a class" in {
        val instance = container.newInstance(classOf[Target])
        instance should not be (null)
      }

      "can delegate instance creation to a special creator" in {
        var called = false
        container.newInstance(classOf[Target], (cl, args) => {
          called = true
          args should be (Seq(a, b))
        })
        called should be (true)

      }
    }

    "with too few references" - {
      val a = new A
      val container = new Container(Seq(a))

      "rejects creation of a target that needs more" in {
        assertThrows[IllegalArgumentException](container.newInstance(classOf[Target]))
      }
    }

    "with a null reference" - {
      val a = new A
      val b: B = null
      val container = new Container(Seq(a, b))

      "rejects creation of a target since null isn't a good fit" in {
        assertThrows[IllegalArgumentException](container.newInstance(classOf[Target]))
      }
    }
  }

}

object ContainerTest {
  class Target(val a: A, val b: B)
  class NoPublicConstructor private()
  class MultiplePublicConstructors() {
    def this(a: A) = this()
  }

  class A
  class B

}
