package fp.coinflip

import CoinFlipUtils._
import scala.annotation.tailrec
import scala.util.Random

case class GameState(numFlips: Int, numCorrect: Int)

object CoinFlip extends App {

  val r = Random
  val s = GameState(0, 0)

  mainLoop(s, r)

  @tailrec
  def mainLoop(s: GameState, r: Random) {

    showPrompt()
    val userInput = getUserInput()

    userInput match {
      case "H" | "T" ⇒ {
        val coinTossResult = flipCoin(r)
        val newNumFlips = s.numFlips + 1

        if (userInput == coinTossResult) {
          val newNumCorrect = s.numCorrect + 1
          val newS = s.copy(numFlips = newNumFlips, numCorrect = newNumCorrect)
          mainLoop(newS, r)
        } else {
          val newS = s.copy(numFlips = newNumFlips)
          printGameState(printableFlipResult(coinTossResult), newS)
          mainLoop(newS, r)
        }
      }
      case _ ⇒ {
        printGameOver()
        printGameState(s)
      }
    }
  }
}

