package sm

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.funsuite.AnyFunSuite

import java.lang

case class Order(id: Int, status: String)

class TransitionTest extends AnyFunSuite, LazyLogging {

  test("init01") {
    val t = Transition("a", "b", (o: Order) => Option(o).exists(_.status == "a"), (o: Order) => Some(o.copy(status = "b")))
    val o = Order(1, "a")
    assert(t.check(o))
    println(t.execute(o))
    assert(t.execute(o).map(o => o.status).orNull() == "b")
  }

  test("initSm01") {
    val t: Transition[String, Order, Order] = Transition("a", "b", (o: Order) => Option(o).exists(_.status == "a"), (o: Order) => o.copy(status = "b"))
    val o = Order(1, "a")
    val sm = StateMachine(s = classOf[String], c = classOf[Order], r = classOf[Order])
      .withTransition(t)
      .withTransition("a", "c", o => Option(o).exists(_.status == "a"), o => o.copy(status = "c"))

    val r1 = sm.mkTransition("a", "b", o)
    assertResult(r1.status)("b")

    val r2 = sm.mkTransition("a", "c", o)
    assertResult(r2.status)("c")
  }

}
