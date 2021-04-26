// Modified Fib problem
// http://rosalind.info/problems/fibd/
object MortalFibonacciRabbits {
  case class State(ageZero: BigInt, ageOne: BigInt, ageAdults: List[BigInt])

  def timeMachine(state: State)(implicit monthLifeTime: Int): State = state match {
    case State(ageZero, ageOne, adults) =>
      val adultsGeneration : List[BigInt]= (ageOne +: adults)
        .take(monthLifeTime - 1)
      State(0, ageZero, adultsGeneration)
  }

  def reproduce(state: State)(k: Int): State = state match {
    case State(ageZero, ageOne, adults) =>
      State( adults.sum * k + ageZero, ageOne, adults)
  }

  def rabbitIterate(state: State, n: Int)
                   (k: Int)(implicit monthLifeTime: Int): State = n match {
    case month if month > 0 =>
      val generation = reproduce(state)(k)
      val aged = timeMachine(generation)
      rabbitIterate(aged, n - 1)(k)(monthLifeTime)
    case _ => state
  }

  def rabbitCount(state: State) : BigInt =
    state.ageZero + state.ageOne + state.ageAdults.sum

}
