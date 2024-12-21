package basic.langs.implicits

import org.scalatest.funsuite.AnyFunSuite

import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.JButton
import scala.language.implicitConversions

class ImplicitConversion extends AnyFunSuite {

  test("javaImplementation") {
    val button = new JButton
    // only parameter of actionEvent and println is new information
    // the rest code lines are all boilerplate
    button.addActionListener(
      new ActionListener {
        override def actionPerformed(e: ActionEvent): Unit = {
          println("pressed!")
        }
      }
    )
    button.doClick()
  }

  implicit def functionToActionListener(function: ActionEvent => Unit): ActionListener = new ActionListener {
    override def actionPerformed(actionEvent: ActionEvent): Unit = function(actionEvent)
  }

  test("scalaImplementation") {
    val button = new JButton
    // scala compiler tries to look for an implicit conversion before it take (_: ActionEvent) => println("pressed") as an error
    button.addActionListener((_: ActionEvent) => println("pressed"))
    button.doClick()
  }

  test("markingRule") {
    /**
     * Marking Rule: Only definitions marked implicit are available
     */
    implicit def intToString(x: Int): String = x.toString
    // number 321 will be changed to String before it goes into concat()
    println("int".concat(321))
  }

  test("scopeRule") {
    /**
     * Scope Rule: An inserted implicit conversion must be in scope as a single identifier, or be associated with the source or target of the conversion
     */
    // compilation error, cause implicit def intToString() is not in scope
    // println("int".concat(321))
  }

  test("one_at_a_time_rule") {
    implicit def intToString(x: Int): String = x.toString
    //One-at-a-time Rule: Only one implicit is tried
    //implicit def anotherIntToString(x: Int): String = x.toString
    println("int".concat(321))
  }

  test("explicitsFirstRule") {
    /**
     * Explicits-First Rule: Whenever code type checks as it is written, no implicits are attempted
     */
    implicit def intToString(x: Int): String = x.toString

    val num = 99
    //no implicit conversion, + is a synthetic build-in operations
    println("int" + (1 + num))
  }


}
