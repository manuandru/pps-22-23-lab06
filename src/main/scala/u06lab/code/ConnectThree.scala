package u06lab.code

import java.util.OptionalInt

// Optional!
object ConnectThree extends App:
  val bound = 3
  val seqForWin = 3
  enum Player:
    case X, O
    def other: Player = this match
      case X => O
      case _ => X

  case class Disk(x: Int, y: Int, player: Player)
  /**
   * Board:
   * y
   *
   * 3 X X O X
   * 2 X O X O
   * 1 O X O X
   * 0 X O X X
   *   0 1 2 3 <-- x
   */
  type Board = Seq[Disk]
  type Game = Seq[Board]

  import Player.*

  def find(board: Board, x: Int, y: Int): Option[Player] = board.collectFirst { case Disk(`x`, `y`, p) => p }

  def firstAvailableRow(board: Board, x: Int): Option[Int] = Some(board.count(_.x == x)).filter(_ <= bound)

  def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    for
      x <- 0 to bound
      y <- firstAvailableRow(board, x)
    yield Disk(x, y, player) +: board

  def computeAnyGame(player: Player, moves: Int): LazyList[Game] = moves match
    case 1 => LazyList(placeAnyDisk(Seq(), player))
    case _ =>
      for
        game <- computeAnyGame(player.other, moves - 1)
        board <- game
        if board.size == moves - 1
      yield
        if win(board)
        then game
        else placeAnyDisk(board, player) ++ game

  def checkRow(board: Board)(xMap: (Int, Int) => Int, yMap: (Int, Int) => Int): Boolean =
    (for
      x <- 0 to bound
      y <- 0 to bound
      player <- find(board, x, y)
      others = (1 until seqForWin).map { i => find(board, xMap(x, i), yMap(y, i)) }
    yield others.forall { _.contains(player) }).exists(x => x)

  def win(board: Board): Boolean =
    val horizontal: Boolean = checkRow(board)((x, _) => x, _ + _)
    val vertical: Boolean = checkRow(board)(_ + _, (y, _) => y)
    val diagonal: Boolean = checkRow(board)(_ + _, _ + _)
    val antiDiagonal: Boolean = checkRow(board)(_ + _, _ - _)
    horizontal || vertical || diagonal || antiDiagonal

  def printBoards(game: Seq[Board]): Unit =
    for
      y <- bound to 0 by -1
      board <- game.reverse
      x <- 0 to bound
    do
      print(find(board, x, y).map(_.toString).getOrElse("."))
      if x == bound then
        print(" ")
        if board == game.head then println()

  // Exercise 1: implement find such that..
  println("EX 1: ")
  println(find(List(Disk(0, 0, X)), 0, 0)) // Some(X)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 0, 1)) // Some(O)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 1, 1)) // None

  // Exercise 2: implement firstAvailableRow such that..
  println("EX 2: ")
  println(firstAvailableRow(List(), 0)) // Some(0)
  println(firstAvailableRow(List(Disk(0, 0, X)), 0)) // Some(1)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X)), 0)) // Some(2)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X)), 0)) // Some(3)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)), 0)) // None
  // Exercise 2: implement placeAnyDisk such that..
  printBoards(placeAnyDisk(List(), X))
  // .... .... .... ....
  // .... .... .... ....
  // .... .... .... ....
  // ...X ..X. .X.. X...
  printBoards(placeAnyDisk(List(Disk(0, 0, O)), X))
  // .... .... .... ....
  // .... .... .... ....
  // ...X .... .... ....
  // ...O ..XO .X.O X..O
  println("EX 3: ")
// Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  println(computeAnyGame(O, 8).size)
//  computeAnyGame(O, 2).foreach { g =>
//    printBoards(g)
//    println()
//  }
//  .... .... .... .... ...O
//  .... .... .... ...X ...X
//  .... .... ...O ...O ...O
//  .... ...X ...X ...X ...X
//
//
// .... .... .... .... O...
// .... .... .... X... X...
// .... .... O... O... O...
// .... X... X... X... X...

// Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
