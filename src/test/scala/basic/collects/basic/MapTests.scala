package basic.collects.basic

import fp.intro.Generator
import org.scalatest.funsuite.AnyFunSuite

import java.util
import scala.collection.immutable.TreeMap
import scala.collection.{immutable, mutable}
import scala.io.{BufferedSource, Source}
import scala.util.{Try, Using}

class MapTests extends AnyFunSuite {

  test("immutableMap") {
    val romanNumeral = immutable.Map(1 -> "I", 2 -> "II", 3 -> "III", 4 -> "IV", 5 -> "V")
    println(romanNumeral(4))
  }

  test("mutableMap") {
    val treasureMap = mutable.Map[Int, String]()
    treasureMap += (1 -> "Go to island.")
    treasureMap += (2 -> "Find big X on ground.")
    treasureMap += (3 -> "Dig.")
    println(treasureMap(2))
    assert(treasureMap(3) == "Dig.")
  }

  test("mutableValueSet") {
    val map = mutable.Map.empty[Int, String]
    map(1) = "one"
    map(2) = "two"
    map(3) = "three"
    assert("one" == map(1))
    assert("two" == map(2))
    assert("three" == map(3))
  }

  test("iterateJavaMap") {
    val map: java.util.Map[Int, String] = new util.HashMap[Int, String]()
    map.put(1, "one")
    map.put(2, "two")
    map.put(3, "three")
    map.forEach((k, v) => println(s"$k->$v"))
  }

  test("countWords") {
    def countWords(text: String): Map[String, Int] = {
      val counts = mutable.Map.empty[String, Int]
      for (word <- text.split("[ ,.!;]+")) if (counts.contains(word)) {
        val count = counts(word) + 1
        counts += (word -> count)
      } else {
        counts += (word -> 1)
      }
      counts.toMap
    }

    assert(Map("See" -> 1, "Spot" -> 2, "Run" -> 3) == countWords("See Spot Run. Run, Spot. Run!"))
    assert(Map("Run" -> 3, "Spot" -> 2, "See" -> 1) == countWords("See Spot Run. Run, Spot. Run!"))
  }


  test("countWords01") {
    //    val buffer: BufferedSource = Source.fromFile("README.md")
    //    buffer.getLines()
    //      .flatMap(_.split("[\\s+,.!;]+"))
    //      .foldLeft(Map.empty[String, Int])((map, word) => map.+(word -> (map.getOrElse(word, 0) + 1)))
    //      .toList.sortBy(_._2)(Ordering[Int].reverse).foreach(println)
    Generator
      .selfClosing {
        val buffer: BufferedSource = Source.fromFile("README.md")
        (buffer.getLines(), () => buffer.close())
      }
      .flatMap(line => Generator(line.split("[\\s+,.!;]").toSeq: _*))
      .foldLeft(Map.empty[String, Int])((map, word) => map.+(word -> (map.getOrElse(word, 0) + 1)))
      .toList.sortBy(_._2)(Ordering[Int].reverse).foreach(println)
  }

  test("countWords02") {
    val lst = Using(Source.fromFile("README.md"))(buffer => buffer.getLines().toList)
      .map(_.flatMap(_.split("[\\s+,.!;]+")))
      .map(_.filter(!_.isBlank))
      .map(_.foldLeft(Map.empty[String, Int])((map, word) => map.+(word -> (map.getOrElse(word, 0) + 1))))
      .map(_.toList.sortBy(_._2)(Ordering[Int].reverse))
      .getOrElse(List.empty)

    lst.foreach(println)
  }

  test("sortedMap") {
    val treeMap = TreeMap(1 -> "HK", 2 -> "TW", 3 -> "MC")(new Ordering[Int] {
      override def compare(x: Int, y: Int): Int = y - x
    })
    treeMap.foreach(pair => println(s"${pair._1}  -> ${pair._2}"))
  }

  test("syntacticSugar") {
    var capital = Map("US" -> "Washington", "France" -> "Paris")
    capital += ("Japan" -> "Tokyo")
    capital.foreach(pair => println(s"${pair._1}  -> ${pair._2}"))
  }

}
