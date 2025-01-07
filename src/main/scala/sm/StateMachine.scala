package sm


sealed case class StateMachine[S, C, R] private(s: Class[S], c: Class[C], r: Class[R], transitions: List[Transition[S, C, R]]) {

  def typeOf(s: Class[S], c: Class[C], r: Class[R]): StateMachine[S, C, R] = StateMachine(s, c, r, List.empty)


}
