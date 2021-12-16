package chess

type BitBoard = Long
type Move = Board => Board

object BitBoard:
  val empty: BitBoard = 0L
  @inline def apply(bitBoards: BitBoard*): BitBoard = bitBoards.foldLeft(0L)(_ | _)

case class Board(
  whiteKing: BitBoard,
  whiteQueens: BitBoard,
  whiteBishops: BitBoard,
  whiteKnights: BitBoard,
  whiteRooks: BitBoard,
  whitePawns: BitBoard,
  blackKing: BitBoard,
  blackQueens: BitBoard,
  blackBishops: BitBoard,
  blackKnights: BitBoard,
  blackRooks: BitBoard,
  blackPawns: BitBoard,
  enPassant: BitBoard,
  castle: BitBoard,
) {
  lazy val whitePieces: BitBoard = whiteKing | whiteQueens | whiteBishops | whiteKnights | whiteRooks | whitePawns
  lazy val blackPieces: BitBoard = blackKing | blackQueens | blackBishops | blackKnights | blackRooks | blackPawns

  @inline def whiteKingCaptures: BitBoard = Board.kingAttacks(whiteKing) & blackPieces

  @inline def xRayCaptures(piecePosition: BitBoard, attacks: Map[BitBoard, BitBoard], isWhitePiece: Boolean): BitBoard =
    val attacked = attacks(piecePosition)
    if isWhitePiece then
      blackPieces & attacked
    else
      whitePieces & attacked
}

extension (boards: List[BitBoard])
  def join: BitBoard = boards.foldLeft(BitBoard.empty)(_ | _)

extension (board: BitBoard)
  @inline def northWest(squares: Int = 1): BitBoard = board << 9 * squares
  @inline def north(squares: Int = 1): BitBoard = board << 8 * squares
  @inline def northEast(squares: Int = 1): BitBoard = board << 7 * squares
  @inline def east(squares: Int = 1): BitBoard = board >> 1 * squares
  @inline def southEast(squares: Int = 1): BitBoard = board >> 9 * squares
  @inline def south(squares: Int = 1): BitBoard = board >> 8 * squares
  @inline def southWest(squares: Int = 1): BitBoard = board >> 7 * squares
  @inline def west(squares: Int = 1): BitBoard = board << 1 * squares
  def show: String =
    val spaceBetweenRankAndBoard = "   "
    val files = (1 to 8).toList.reverse
    val ranks = Iterator("", s" $spaceBetweenRankAndBoard${('A' to 'H').mkString(" ")}")
    /*val ranks = ('A' to 'H').toList.reverse
    val files = Iterator("", s" $spaceBetweenRankAndBoard${(1 to 8).mkString(" ")}")*/
    (board.toBinaryString.reverse.padTo(64, '0').reverse.grouped(8).zip(files).map { (file, fileNumber) =>
      s"$fileNumber$spaceBetweenRankAndBoard${file.mkString(" ")}"
    } ++ ranks).mkString("\n")
  @inline def bits: Int =
    var currentBoard = board
    var bits = 0
    while (currentBoard != 0)
      bits += 1
      currentBoard &= currentBoard - 1
    bits
  /**
   * A board with only the least significant bit
   */
  @inline def firstBit: BitBoard = board & -board
  @inline def firstBitIndex: Option[Int] = Option.when(board != 0)((board.firstBit - 1).bits)
  @inline def firstBitRemoved: BitBoard = board & board - 1
  @inline def attacks(attackMap: Map[BitBoard, BitBoard]) =
    var currentBoard = BitBoard.empty
    var remainingPieces = board
    while (remainingPieces != 0)
      val onlyFirstBit = remainingPieces.firstBit
      currentBoard |= attackMap(onlyFirstBit)
      remainingPieces -= onlyFirstBit
    currentBoard

object Board:
  val allBoardPositions: List[BitBoard] = List.tabulate(64)(1L << _)

  val lineMasks: Map[BitBoard, BitBoard] =
    val lineMasks: Array[BitBoard] = Array.tabulate(8)(0xFFL << _ * 8)
    allBoardPositions.map { position =>
      position -> lineMasks.find(mask => (mask & position) != 0).get
    }.toMap

  val distances: Map[BitBoard, (Int, Int, Int, Int)] = allBoardPositions.map { position =>
    val exponent = (math.log(math.abs(position.toDouble)) / math.log(2)).toInt
    val distanceFromRight = exponent % 8
    val distanceFromBottom = exponent / 8
    position -> (7 - distanceFromBottom, distanceFromBottom, distanceFromRight, 7 - distanceFromRight)
  }.toMap

  private def rookBitBoards(tillTheEdge: Boolean): Map[BitBoard, BitBoard] = allBoardPositions.map { position =>
    val (north, south, east, west) = distances(position)
    lazy val offset = if tillTheEdge then 1 else 0
    lazy val drops = if tillTheEdge then 0 else 1

    val mask = List.tabulate(north)(distance => position.north(distance + offset)).drop(drops).join |
      List.tabulate(south)(distance => position.south(distance + offset)).drop(drops).join |
      List.tabulate(east)(distance => position.east(distance + offset)).drop(drops).join |
      List.tabulate(west)(distance => position.west(distance + offset)).drop(drops).join

    position -> mask
  }.toMap

  def bishopBitBoards(tillTheEdge: Boolean): Map[BitBoard, BitBoard] = allBoardPositions.map { position =>
    val (north, south, east, west) = distances(position)
    lazy val offset = if tillTheEdge then 1 else 0
    lazy val drops = if tillTheEdge then 0 else 1

    val mask = List.tabulate(north min west)(distance => position.northWest(distance + offset)).drop(drops).join |
      List.tabulate(north min east)(distance => position.northEast(distance + offset)).drop(drops).join |
      List.tabulate(south min west)(distance => position.southWest(distance + offset)).drop(drops).join |
      List.tabulate(south min east)(distance => position.southEast(distance + offset)).drop(drops).join

    position -> mask
  }.toMap

  private def pawnMoves(move: (BitBoard, Int) => BitBoard): Map[BitBoard, BitBoard] =
    val fromStart = List.tabulate(8) { i =>
      val startingPosition = move(i, 1)
      startingPosition -> (move(startingPosition, 1) | move(startingPosition, 2))
    }
    val others = List.tabulate(48) { i =>
      val position = move(i, 2)
      position -> move(position, 1)
    }
    (fromStart ++ others).toMap

  private def pawnAttacks(move1: BitBoard => BitBoard, move2: BitBoard => BitBoard): Map[BitBoard, BitBoard] =
    List.tabulate(56) { position =>
      position.toLong -> (move1(position) | move2(position))
    }.toMap



  val kingAttacks: Map[BitBoard, BitBoard] = allBoardPositions.map { position =>
    val attacks =
      position.northWest()   | position.north() | position.northEast() |
        position.west()                         | position.east() |
        position.southWest() | position.south() | position.southEast()
    position -> attacks
  }.toMap

  val rookAttacks: Map[BitBoard, BitBoard] = rookBitBoards(true)

  val bishopAttacks: Map[BitBoard, BitBoard] = bishopBitBoards(true)

  val queenAttacks: Map[BitBoard, BitBoard] = allBoardPositions.map { position =>
    position -> (rookAttacks(position) | bishopAttacks(position))
  }.toMap

  val knightAttacks: Map[BitBoard, BitBoard] = allBoardPositions.map { position =>
    val (north, south, east, west) = distances(position)

    val northMoves = if north >= 2 then
      (if east >= 1 then position.northEast().north() else 0L) | (if west >= 1 then position.northWest().north() else 0L)
    else 0

    val southMoves = if south >= 2 then
      (if east >= 1 then position.southEast().south() else 0L) | (if west >= 1 then position.southWest().south() else 0L)
    else 0

    val eastMoves = if east >= 2 then
      (if north >= 1 then position.northEast().east() else 0L) | (if south >= 1 then position.southEast().east() else 0L)
    else 0

    val westMoves = if west >= 2 then
      (if north >= 1 then position.northWest().west() else 0L) | (if south >= 1 then position.southWest().west() else 0L)
    else 0

    position -> (northMoves | southMoves | eastMoves | westMoves)
  }.toMap

  val whitePawnMoves: Map[BitBoard, BitBoard] = pawnMoves((board, squares) => board.north(squares))
  val blackPawnMoves: Map[BitBoard, BitBoard] = pawnMoves((board, squares) => board.south(squares))

  val whitePawnAttacks: Map[BitBoard, BitBoard] = pawnAttacks(_.northWest(), _.northEast()).map((position, attacks) => position.north() -> attacks.north())
  val blackPawnAttacks: Map[BitBoard, BitBoard] = pawnAttacks(_.southWest(), _.southEast())

  // These are attacks where the ray doesn't go to the edge of the board
  val rookRelevantOccupancy: Map[BitBoard, BitBoard] = rookBitBoards(false)
  val bishopRelevantOccupancy: Map[BitBoard, BitBoard] = bishopBitBoards(false)
