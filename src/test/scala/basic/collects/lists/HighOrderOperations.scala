package basic.collects.lists

import scala.language.postfixOps
import org.scalatest.funsuite.AnyFunSuite

class HighOrderOperations extends AnyFunSuite {

  test("Map") {
    println(List("the", "quick", "brown", "fox").map(_.length).mkString(","))
    println(List("the", "quick", "brown", "fox").map(_.reverse).mkString(","))
  }

  test("flatMap") {
    println(List("the", "quick", "brown", "fox").flatMap(_.toList).mkString(","))
    println(List("the", "quick", "brown", "fox").flatMap(identity(_)).mkString(","))
    println(List("the", "quick", "brown", "fox").flatMap(s => List(s)).flatMap(identity(_)).mkString(","))
  }

  test("filter") {
    assert(List(2, 4) == List(1, 2, 3, 4, 5).filter(_ % 2 == 0))
  }

  test("collect") {
    val list = List("the", "quick", "brown", "fox", "jump", "over", "the", "lazy", "dog")
    val map = list.collect({ case word if word.length > 4 => (word, word.length) }).toMap
    assert(Map("quick" -> 5, "brown" -> 5) == map)
  }

  test("parMap") {
    def doIt(x: Int): Int = {
      println(s"calc $x in ${Thread.currentThread().getName}")
      x * 2
    }
    // import scala.collection.parallel.CollectionConverters._
    // scala-parallel-collections should be imported before par can be used
    // List(1, 2, 3, 4, 5, 6, 7, 8, 9, 0).par.map(doIt);
  }

  test("groupBy") {
    assert(Map(true -> List(2, 4), false -> List(1, 3, 5)) == List(1, 2, 3, 4, 5).groupBy(_ % 2 == 0))
  }

  test("partition") {
    assert((List(2, 4), List(1, 3, 5)) == List(1, 2, 3, 4, 5).partition(_ % 2 == 0))
  }

  test("find") {
    assert(2 == List(1, 2, 3, 4, 5).find(_ % 2 == 0).get)
  }

  test("takeWhile") {
    assert(List(1, 2) == List(1, 2, 3, 4, 5).takeWhile(_ < 3))
  }

  test("dropWhile") {
    assert(List(4, 5) == List(1, 2, 3, 4, 5).dropWhile(_ <= 3))
  }

  test("sliding") {
    assert(List(List(1, 2), List(2, 3)) == List(1, 2, 3, 5, 7, 9).sliding(2, 1).filter(e => e(1) - e.head <= 1).toList)
  }

  test("span") {
    assert((List(1, 2), List(3, 4, 5)) == List(1, 2, 3, 4, 5).span(_ < 3))
  }

  test("forall") {
    assert(List(1, 2, 3).forall(_ > 0))
  }

  test("exists") {
    assert(List(-1, 0, 1).exists(_ > 0))
  }


  test("sortWith") {
    assert(List("apple", "boy", "cart", "friday").sortWith(_.length < _.length) == List("boy", "cart", "apple", "friday"))
  }

  test("reduce") {
    // ((1+2)*2 + 3)*2
    assert(18 == List(1, 2, 3).reduce((left, right) => (left + right) * 2))
    assert(18 == List(1, 2, 3).reduce(_ * 2 + _ * 2))

  }

  test("scan") {
    // 0
    // 0+1
    // 0+1+2
    // 0+1+2+3
    // 0+1+2+3+4
    // 0+1+2+3+4+5
    assert(List(0, 1, 3, 6, 10, 15) == (1 to 5).scan(0)(_ + _))
    assert(List(0, 1, 3, 6, 10, 15) == (1 to 5).scanLeft(0)(_ + _))
    // 0+5+4+3+2+1
    // 0+5+4+3+2
    // 0+5+4+3
    // 0+5+4
    // 0+5
    // 0
    assert(Seq(15, 14, 12, 9, 5, 0) == (1 to 5).scanRight(0)(_ + _))
  }

  test("cons") {
    var l = List(0)
    l ::= 1
    assert(1 :: List(0) == l)
  }

}
