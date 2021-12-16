package chess

import chess.Board.*
import chess.Helpers.*
import chess.MagicNumbers.*

object Main extends App {

  @main def test =
    //bishopRelevantBits.values.foreach(println)
    val position = 1L << 63
    //println(position.show)
    //println(allBoardPositions.contains(0L))
    //println(distances(position))
    //println(bishopRelevantOccupancy.keys.toList.contains(position))
    //println(bishopRelevantOccupancy(position).show)

    allBoardPositions.foreach { position =>
      println(s"position = ${position}")
      println(findMagicNumber(position, rookRelevantBits, rookRelevantOccupancy, rookAttacks))
    }

}
