package fp.coinflip

import scala.util.Random
import scala.io.StdIn.readLine

object CoinFlipUtils {

  def showPrompt(): Unit = print("\n(h)eads (t)ails or (q)uit: ")

  def getUserInput(): String = readLine.trim.toUpperCase()

  def printableFlipResult(flip: String): String = flip match {
    case "H" ⇒ "Heads"
    case "T" ⇒ "Tails"
  }

  def printGameState(printableFlipResult: String, gameState: GameState): Unit = {
    println(s"Flip was $printableFlipResult")
    print(gameState)
  }

  def printGameState(gameState: GameState): Unit =
    print(s"#Flips: ${gameState.numFlips}, #Correct: ${gameState.numCorrect}")

  def printGameOver(): Unit = print("\n==== Game Over ====")

  def flipCoin(r: Random): String = {
    val i = r.nextInt(2)
    i match {
      case 0 ⇒ "H"
      case 1 ⇒ "T"
    }
  }
}
