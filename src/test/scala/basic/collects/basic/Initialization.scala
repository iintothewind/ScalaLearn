package basic.collects.basic

import org.scalatest.funsuite.AnyFunSuite;
import scala.collection.immutable.TreeSet
import scala.collection.{immutable, mutable}

class Initialization extends AnyFunSuite {

  test("explicitType") {
    val colors = List[String]("blue", "green", "red")
  }

  test("convert") {
    val colors = List[String]("blue", "green", "red")
    val treeSet = TreeSet.empty[String] ++ colors
    val list = treeSet.toList
    val array = treeSet.toArray
  }

  test("convertBetweenMutableAndImmutable") {
    val colors = List("blue", "green", "red")
    val immutableColors = immutable.Set.empty[String] ++ colors
    val mutableColors = mutable.Set.empty[String] ++ colors
    val anotherImmutableColors = immutable.Set.empty[String] ++ mutableColors
    val alternativeImmutableColors = mutableColors.toSet
    assert(immutableColors == mutableColors)
    assert(immutableColors == anotherImmutableColors)
    assert(immutableColors == alternativeImmutableColors)
  }

}
