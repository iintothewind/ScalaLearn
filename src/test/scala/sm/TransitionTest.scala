package sm

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.funsuite.AnyFunSuite

import java.lang

case class Order(id: Int, name: String, status: String)

class TransitionTest extends AnyFunSuite, LazyLogging {

  test("init01") {
    val t = Transition("a", "b", (o: Order) => o.status == "a", (o: Order) => Some(o.copy(status = "b")))
    val o = Order(1, "00a1", "a")
    assert(t.check(o))
    println(t.execute(o))
    assert(t.execute(o).map(o => o.status).orNull() == "b")
  }

}
