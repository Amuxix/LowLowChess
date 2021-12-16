package chess

import scala.util.Random

object Helpers:
  val h1: BitBoard = 1L << 0
  val g1: BitBoard = 1L << 1
  val f1: BitBoard = 1L << 2
  val e1: BitBoard = 1L << 3
  val d1: BitBoard = 1L << 4
  val c1: BitBoard = 1L << 5
  val b1: BitBoard = 1L << 6
  val a1: BitBoard = 1L << 7
  val h2: BitBoard = 1L << 8
  val g2: BitBoard = 1L << 9
  val f2: BitBoard = 1L << 10
  val e2: BitBoard = 1L << 11
  val d2: BitBoard = 1L << 12
  val c2: BitBoard = 1L << 13
  val b2: BitBoard = 1L << 14
  val a2: BitBoard = 1L << 15
  val h3: BitBoard = 1L << 16
  val g3: BitBoard = 1L << 17
  val f3: BitBoard = 1L << 18
  val e3: BitBoard = 1L << 19
  val d3: BitBoard = 1L << 20
  val c3: BitBoard = 1L << 21
  val b3: BitBoard = 1L << 22
  val a3: BitBoard = 1L << 23
  val h4: BitBoard = 1L << 24
  val g4: BitBoard = 1L << 25
  val f4: BitBoard = 1L << 26
  val e4: BitBoard = 1L << 27
  val d4: BitBoard = 1L << 28
  val c4: BitBoard = 1L << 29
  val b4: BitBoard = 1L << 30
  val a4: BitBoard = 1L << 31
  val h5: BitBoard = 1L << 32
  val g5: BitBoard = 1L << 33
  val f5: BitBoard = 1L << 34
  val e5: BitBoard = 1L << 35
  val d5: BitBoard = 1L << 36
  val c5: BitBoard = 1L << 37
  val b5: BitBoard = 1L << 38
  val a5: BitBoard = 1L << 39
  val h6: BitBoard = 1L << 40
  val g6: BitBoard = 1L << 41
  val f6: BitBoard = 1L << 42
  val e6: BitBoard = 1L << 43
  val d6: BitBoard = 1L << 44
  val c6: BitBoard = 1L << 45
  val b6: BitBoard = 1L << 46
  val a6: BitBoard = 1L << 47
  val h7: BitBoard = 1L << 48
  val g7: BitBoard = 1L << 49
  val f7: BitBoard = 1L << 50
  val e7: BitBoard = 1L << 51
  val d7: BitBoard = 1L << 52
  val c7: BitBoard = 1L << 53
  val b7: BitBoard = 1L << 54
  val a7: BitBoard = 1L << 55
  val h8: BitBoard = 1L << 56
  val g8: BitBoard = 1L << 57
  val f8: BitBoard = 1L << 58
  val e8: BitBoard = 1L << 59
  val d8: BitBoard = 1L << 60
  val c8: BitBoard = 1L << 61
  val b8: BitBoard = 1L << 62
  val a8: BitBoard = 1L << 63

  /**
   * This generates a random 64 bit number with a low number of bits set
   */
  def generateMagicNumberCandidate: Long =
    def random =
      val u1 = Random.nextInt() & 0xFFFF
      val u2 = Random.nextInt() & 0xFFFF
      val u3 = Random.nextInt() & 0xFFFF
      val u4 = Random.nextInt() & 0xFFFF
      u1 | (u2 << 16) | (u2 << 32) | (u2 << 48)
    random & random & random