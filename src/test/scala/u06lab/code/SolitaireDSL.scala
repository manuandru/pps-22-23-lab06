package u06lab.code

enum Content:
  case Empty  // X
  case Number(n: Int = 0) //N
  case Safe   // S

class Board:
  import Content.*
  def X(using builder: BoardBuilder): Board =
    builder.add(Empty); this
  def N(using builder: BoardBuilder): Board =
    builder.add(Number()); this
  def S(using builder: BoardBuilder): Board =
    builder.add(Safe); this

class BoardBuilder(width: Int, height: Int):
  private val position: Iterator[Position] =
    (for i <- 0 until width; j <- 0 until height yield (i, j)).iterator
  var solution: Map[Position, Content] = Map()
  def add(content: Content): Unit = solution = solution + (position.next -> content)