package basic.collects.basic


import org.scalatest.funsuite.AnyFunSuite
import scala.collection.immutable.TreeSet
import scala.collection.{immutable, mutable}


class SetTests extends AnyFunSuite {

  test("immutableSet") {
    val jetSet = immutable.Set("Boeing", "Airbus")
    val jets = jetSet + "Cessna"
    assert(jets.contains("Cessna"))
    assertResult(3)(jets.size)
  }

  test("immutableHashSet") {
    val hashSet = immutable.HashSet("Tomatoes", "Chilies")
    println(s"$hashSet Coriander")
  }

  test("distinct") {
    val base = immutable.Set.empty[String]
    val words = "See Spot Run. Run, Spot. Run!".split("[ !,.]+")

    def distinct(words: Array[String]): immutable.Set[String] = words.foldLeft(immutable.Set.empty[String])(_ + _)

    assert(Set("See", "Spot", "Run") == distinct(words))
    assert(Set("See", "Spot", "Run") == base ++ words)
  }

  test("mutableSet") {
    val jetSet = mutable.Set("Boeing", "Airbus")
    jetSet += "Lear"
    jetSet += "Lear"
    assert(!jetSet.contains("Cessna"))
    assertResult(3)(jetSet.size)
  }

  test("remove") {
    val nums = Set(1, 2, 3)
    assert(Set(1, 2) == nums - 3)
  }

  test("addMulti") {
    val nums = Set(1, 2, 3)
    assert(Set(1, 2, 3, 4, 5) == nums ++ List(4, 4, 5, 5))
  }

  test("removeMulti") {
    val nums = Set(1, 2, 3)
    assert(Set(1) == nums -- List(2, 2, 3, 3))
  }

  test("intersect") {
    val nums1 = Set(1, 2, 3)
    val nums2 = Set(2, 3, 4, 5)
    assert(Set(2, 3) == (nums1 & nums2))
  }

  test("mutableOperations") {
    val words = mutable.Set.empty[String]
    words += "java"
    words -= "java"
    words ++= List("do", "re", "mi")
    words --= List("do", "re", "mi")
    words += "scala"
    words.clear()
    assert(0 == words.size)
  }

  test("treeSet") {
    val treeSet = TreeSet(9, 8, 5, 4, 8, 7, 6, 2, 3, 1)(new Ordering[Int] {
      override def compare(x: Int, y: Int): Int = y - x
    })
    println(treeSet)
  }

  test("syntacticSugar") {
    val people = Set.empty[String]
    //+= is not supported by immutable set
    //people += "Ada"
    var country = Set.empty[String]
    country += "Agron"
    country += "Belish"
    assert(Set("Agron", "Belish") == country)
  }
}
