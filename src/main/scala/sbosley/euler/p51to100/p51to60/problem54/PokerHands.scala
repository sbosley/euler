package sbosley.euler.p51to100.p51to60.problem54

import scala.annotation.tailrec
import scala.io.Source

object PokerHands {

  def main(args: Array[String]): Unit = {
    val count = Source.fromFile("src/main/scala/sbosley/euler/p51to100/p51to60/problem54/p054_poker.txt").getLines
      .count(playerHandsLine => {
        val playerHands = parsePlayerHands(playerHandsLine)
        playerHands.player1 > playerHands.player2
      })

    println(count)
  }

  private def isStraight(cards: Seq[Int]): Boolean = {
    val sorted = cards.sorted
    cards.length == 5 && (sorted.min to sorted.max) == cards.sorted
  }

  private def parsePlayerHands(line: String): PlayerHands = {
    val cards = line.split(" ")
    PlayerHands(parseHand(cards.slice(0, 5)), parseHand(cards.slice(5, 10)))
  }

  private def parseHand(cards: Array[String]): Hand = {
    cards.foldLeft(Hand(Map.empty, Map.empty)) { (hand, card) =>
      val cardVal = getCardValue(card.head)
      val cardSuit = getCardSuit(card.last)

      val newBySuit = hand.bySuit.getOrElse(cardSuit, Set.empty) + cardVal
      val newByValue = hand.byValue.getOrElse(cardVal, Set.empty) + cardSuit

      hand.copy(hand.bySuit + (cardSuit -> newBySuit), hand.byValue + (cardVal -> newByValue))
    }
  }

  private def getCardSuit(c: Char): Suit = c match {
    case 'S' => Spades
    case 'C' => Clubs
    case 'H' => Hearts
    case 'D' => Diamonds
  }

  private def getCardValue(c: Char): Int = c match {
    case 'A' => 14
    case 'K' => 13
    case 'Q' => 12
    case 'J' => 11
    case 'T' => 10
    case x => x.asDigit
  }

  sealed trait Suit
  case object Hearts extends Suit
  case object Diamonds extends Suit
  case object Clubs extends Suit
  case object Spades extends Suit

  sealed abstract class HandScore[T <: HandScore[T]](val ordinal: Int) extends Ordered[HandScore[_]] {

    def compareToSelfType(other: T): Int

    override def compare(that: HandScore[_]): Int = {
      val result = ordinal.compareTo(that.ordinal)
      if (result == 0) compareToSelfType(that.asInstanceOf[T])
      else result
    }
  }
  case class OnePair(pairVal: Int) extends HandScore[OnePair](0) {
    override def compareToSelfType(other: OnePair): Int = pairVal.compareTo(other.pairVal)
  }
  case class TwoPair(pair1: Int, pair2: Int) extends HandScore[TwoPair](1) {
    override def compareToSelfType(other: TwoPair): Int = {
      val maxResult = Math.max(pair1, pair2).compareTo(Math.max(other.pair1, other.pair2))
      if (maxResult == 0) Math.min(pair1, pair2).compareTo(Math.min(other.pair1, other.pair2))
      else maxResult
    }
  }
  case class ThreeOfAKind(trieValue: Int) extends HandScore[ThreeOfAKind](2) {
    override def compareToSelfType(other: ThreeOfAKind): Int = trieValue.compareTo(other.trieValue)
  }
  case class Straight(highVal: Int) extends HandScore[Straight](3) {
    override def compareToSelfType(other: Straight): Int = highVal.compareTo(other.highVal)
  }
  case class Flush(suit: Suit) extends HandScore[Flush](4) {
    override def compareToSelfType(other: Flush): Int = 0
  }
  case class FullHouse(trieValue: Int, pairValue: Int) extends HandScore[FullHouse](5) {
    override def compareToSelfType(other: FullHouse): Int = {
      val trieResult = trieValue.compareTo(other.trieValue)
      if (trieResult == 0) pairValue.compareTo(other.pairValue)
      else trieResult
    }
  }
  case class FourOfAKind(fourValue: Int) extends HandScore[FourOfAKind](6) {
    override def compareToSelfType(other: FourOfAKind): Int = fourValue.compareTo(other.fourValue)
  }
  case class StraightFlush(suit: Suit, highValue: Int) extends HandScore[StraightFlush](7) {
    override def compareToSelfType(other: StraightFlush): Int = highValue.compareTo(other.highValue)
  }
  case class RoyalFlush(suit: Suit) extends HandScore[RoyalFlush](8) {
    override def compareToSelfType(other: RoyalFlush): Int = 0
  }

  case class PlayerHands(player1: Hand, player2: Hand)
  case class Hand(bySuit: Map[Suit, Set[Int]], byValue: Map[Int, Set[Suit]]) extends Ordered[Hand] {
    private def royalFlush: Option[RoyalFlush] = {
      bySuit.find { case (_, cards) => cards == Set(14, 13, 12, 11, 10) }.map(x => RoyalFlush(x._1))
    }
    private def straightFlush: Option[StraightFlush] = {
      bySuit.find { case (_, cards) => isStraight(cards.toSeq) }
        .map { case (suit, cards) => StraightFlush(suit, cards.max) }
    }
    private def fourOfAKind: Option[FourOfAKind] = {
      byValue.find { case (_, suits) => suits.size == 4 }.map(x => FourOfAKind(x._1))
    }
    private def fullHouse: Option[FullHouse] = {
      byValue.find(_._2.size == 3)
        .flatMap(trieValue => byValue.find(_._2.size == 2)
          .map(pairValue => FullHouse(trieValue._1, pairValue._1)))
    }
    private def flush: Option[Flush] = {
      bySuit.find(_._2.size == 5).map(x => Flush(x._1))
    }
    private def straight: Option[Straight] = {
      if (isStraight(byValue.keys.toSeq)) Some(Straight(byValue.keys.max))
      else None
    }
    private def threeOfAKind: Option[ThreeOfAKind] = {
      byValue.find(_._2.size == 3).map(x => ThreeOfAKind(x._1))
    }
    private def twoPair: Option[TwoPair] = {
      val firstPair = byValue.find(_._2.size == 2).map(_._1)
      val secondPair = firstPair
        .flatMap(pair1 => byValue.find { case (value, suits) => value != pair1 && suits.size == 2 }.map(_._1))
      firstPair.flatMap(pair1 => secondPair.map(pair2 => TwoPair(pair1, pair2)))
    }
    private def onePair: Option[OnePair] = {
      byValue.find(_._2.size == 2).map(x => OnePair(x._1))
    }

    def getScore: Option[HandScore[_]] = {
      royalFlush orElse
        straightFlush orElse
        fourOfAKind orElse
        fullHouse orElse
        flush orElse
        straight orElse
        threeOfAKind orElse
        twoPair orElse
        onePair
    }

    override def compare(that: Hand): Int = {
      val compareScores = (getScore, that.getScore) match {
        case (Some(x), Some(y)) => x.compare(y)
        case (Some(_), None) => 1
        case (None, Some(_)) => -1
        case (None, None) => 0
      }
      if (compareScores == 0) compareHighCards(that) else compareScores
    }

    private def compareHighCards(that: Hand): Int = {
      val myCards = bySuit.values.flatMap(_.toSeq).toSeq.sorted.reverse
      val otherCards = that.bySuit.values.flatMap(_.toSeq).toSeq.sorted.reverse
      if (myCards.length != 5 || otherCards.length != 5) throw new IllegalStateException("Card counting went wrong")
      compareCardArrays(myCards, otherCards)
    }

    @tailrec
    private def compareCardArrays(p1: Seq[Int], p2: Seq[Int]): Int = {
      if (p1.isEmpty) 0
      else {
        val compareHighest = p1.head.compareTo(p2.head)
        if (compareHighest == 0) compareCardArrays(p1.tail, p2.tail)
        else compareHighest
      }
    }
  }

}
