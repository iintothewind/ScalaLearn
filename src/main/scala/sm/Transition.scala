package sm

import java.util.Objects


sealed case class Transition[S, C, R](fromState: S, toState: S, predicate: C => Boolean, converter: C => Option[R]) {

  private def getFromState: S = fromState

  private def getToState: S = toState

  override def hashCode(): Int = Objects.hash(fromState, toState)

  override def equals(obj: Any): Boolean = {
    obj match {
      case that: Transition[_, _, _] => Objects.equals(fromState, that.getFromState) && Objects.equals(toState, that.getToState)
      case _ => false
    }
  }

  def check[C1 >: C](context: C): Boolean = Option(predicate) match {
    case Some(p) => p(context)
    case _ => false
  }

  def execute[C1 >: C, R1 <: R](context: C): Option[R] = Option(converter) match {
    case Some(f) => if (check(context)) f(context) else None
    case _ => None
  }
}

