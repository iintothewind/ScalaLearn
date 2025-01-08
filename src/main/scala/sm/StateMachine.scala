package sm

case class StateMachine[S, C, R](s: Class[S], c: Class[C], r: Class[R], transitions: Set[Transition[S, C, R]] = Set.empty[Transition[S, C, R]]) {

  def withTransition(from: S, to: S, predicate: C => Boolean, exec: C => R): StateMachine[S, C, R] = {
    if (transitions.exists(t => t.fromState == from && t.toState == to)) {
      throw new IllegalStateException(s"transition from: $from to: $to already exists")
    } else {
      val lst = transitions.incl(Transition(from, to, predicate, exec))
      copy(transitions = lst)
    }
  }

  def withTransition(transition: Transition[S, C, R]): StateMachine[S, C, R] = Option(transition) match
    case Some(t) => withTransition(t.fromState, t.toState, t.predicate, t.executor)
    case _ => this

  def withTransition(from: S, to: S, exec: C => R): StateMachine[S, C, R] = withTransition(from, to, _ => true, exec)

  def withTransitions(trans: List[Transition[S, C, R]]): StateMachine[S, C, R] = Option(trans) match
    case Some(lst) if (lst.toSet.intersect(transitions).isEmpty) => copy(transitions = transitions.concat(lst))
    case _ => this

  def withTransitions(fromStates: List[S], to: S, predicate: C => Boolean, exec: C => R): StateMachine[S, C, R] = Option(fromStates) match
    case Some(lst) => withTransitions(lst.map(from => Transition(from, to, predicate, exec)))
    case _ => this

  def withTransitions(from: S, toStates: List[S], predicate: C => Boolean, exec: C => R): StateMachine[S, C, R] = Option(toStates) match
    case Some(lst) => withTransitions(lst.map(to => Transition(from, to, predicate, exec)))
    case _ => this

  def change(fromState: S, toState: S, context: C): R = transitions.find(t => t.fromState == fromState && t.toState == toState).map(t => t.execute(context)).getOrElse(null.asInstanceOf[R])

}

