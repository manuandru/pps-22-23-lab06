package u06lab.code

import org.junit.Test
import org.junit.Assert.*

class SolitaireTest:

  @Test def isSafeTest =
    given builder: BoardBuilder = BoardBuilder(5, 7)
    val center = (2, 3)
    // A sort of DSL: X = Empty, N = Number, S = Safe for N
    Board()
      .X.S.X.X.X.S.X
      .X.X.X.X.X.X.X
      .S.X.X.N.X.X.S
      .X.X.X.X.X.X.X
      .X.S.X.X.X.S.X

    builder.solution.keySet
      .foreach(p =>
        if builder.solution(p) == Content.Safe
        then assertTrue(isSafe(p, center))
        else assertFalse(isSafe(p, center))
      )
