package u06lab.code

import org.junit.Test
import org.junit.Assert.*

enum Content:
  case Empty    // X = Empty
  case Number   // N = Number
  case Safe     // S = Safe for N

class Board:
  import Content.*
  def X(using builder: BoardBuilder): Board =
    builder.add(Empty); this
  def N(using builder: BoardBuilder): Board =
    builder.add(Number); this
  def S(using builder: BoardBuilder): Board =
    builder.add(Safe); this

class BoardBuilder(width: Int, height: Int):
  private val position: Iterator[Position] =
    (for i <- 0 until width; j <- 0 until height yield (i, j)).iterator
  var solution: Map[Position, Content] = Map()
  def add(content: Content): Unit = solution = solution + (position.next -> content)

class SolitaireTest:

  @Test def isSafeTest =
    val center = (2, 3)
    given builder: BoardBuilder = BoardBuilder(5, 7)
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
