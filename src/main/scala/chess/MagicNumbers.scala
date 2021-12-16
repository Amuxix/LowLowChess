package chess

import chess.Board.*
import chess.Helpers.*

object MagicNumbers {
  lazy val bishopRelevantBits = allBoardPositions.zip(List(
    6, 5, 5, 5, 5, 5, 5, 6,
    5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 7, 7, 7, 7, 5, 5,
    5, 5, 7, 9, 9, 7, 5, 5,
    5, 5, 7, 9, 9, 7, 5, 5,
    5, 5, 7, 7, 7, 7, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5,
    6, 5, 5, 5, 5, 5, 5, 6,
  )).toMap

  lazy val rookRelevantBits = allBoardPositions.zip(List(
    12, 11, 11, 11, 11, 11, 11, 12,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    12, 11, 11, 11, 11, 11, 11, 12,
  )).toMap

  extension (board: BitBoard)
    private def setOccupancy(index: Int, bitsInMask: Int): BitBoard =
      (0 to bitsInMask).foldLeft((BitBoard.empty, board)) {
        case ((occupancy, mask), count) =>
          val bit = mask.firstBit
          val newOccupancy = occupancy | (if (index & 1 << count) != 0 then bit else 0L)
          val newMask = ~bit & mask
          (newOccupancy, newMask)
      }._1


  //def findMagicNumber(position: BitBoard, bitsMaskAndAttacks: BitBoard => (Int, BitBoard, BitBoard)): Option[Long] =
  def findMagicNumber(position: BitBoard, relevantBitsF: BitBoard => Int, attackMaskF: BitBoard => BitBoard, pieceAttacksF: BitBoard => BitBoard): Option[Long] =
    lazy val relevantBits = relevantBitsF(position)
    lazy val attackMask = attackMaskF(position)
    lazy val pieceAttacks = pieceAttacksF(position)

    var usedAttacks: Map[Long, Long] = Map.empty

    val occupancyIndexes = 1 << relevantBits

    val (occupancies, attacks) = (0 to occupancyIndexes).foldLeft((Map.empty[Int, Long], Map.empty[Int, Long])) {
      case ((occupancies, attacks), index) =>
        (
          occupancies + (index -> attackMask.setOccupancy(index, relevantBits)),
          attacks + (index -> pieceAttacks)
        )
    }

    val magicMask: Long = 0xFF00000000000000

    def isMagicNumberValid(magicNumber: Long): Boolean =
      lazy val isValidCandidate = ((magicNumber * attackMask) & magicMask).bits > 6
      lazy val isValid = (0 to occupancyIndexes).foldLeft(true) {
        case (false, _) => false
        case (_, index) =>
          val magicIndex = (occupancies(index) * magicNumber) >> (64 - relevantBits)
          if !usedAttacks.contains(magicIndex) then
            usedAttacks += (magicIndex -> attacks(index))
            true
          else if !usedAttacks.get(magicIndex).contains(attacks(index)) then
          //Magic number is invalid
            false
          else
            true
      }
      isValidCandidate && isValid

    Iterator.continually(generateMagicNumberCandidate).find(isMagicNumberValid)
}
