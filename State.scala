package connectfour

import java.io.FileNotFoundException
import java.io.PrintWriter
import java.io.UnsupportedEncodingException
import scala.beans.BeanProperty
import State._
import scala.collection.mutable.ArrayBuffer

class State(@BeanProperty var player: Player, @BeanProperty var board: Board, @BeanProperty var lastMove: Move)
  extends Comparable[Any] {

  @BeanProperty
  var children: Array[State] = length0

  @BeanProperty
  var value: Int = 0

  /**
   * Retrieves the possible moves and initializes this State's children.
   * The result is that this State's children reflect the possible
   * States that can exist after the next move. Remember, in the
   * children it is the opposite player's turn. This method
   * initializes only this State's children; it does not recursively
   * initialize all descendants.
   */
  def initializeChildren(): Unit = {

    val states = scala.collection.mutable.ArrayBuffer.empty[State]
    val possibleMoves: Array[Move] = board.getPossibleMoves(player)
    
    possibleMoves.foreach(possMove => {
      val boardCopy = new Board(board, possMove)
      if (this.getPlayer.equals(player))
        states += new State(player.opponent, boardCopy, possMove)
      else
        states += new State(player, boardCopy, possMove)
    })
    children = states.toArray
    // this successfully prints current state and direct children nodes (one level down)
    // writeToFile() 
  }   

def writeToFile() {
    var writer: PrintWriter = new PrintWriter("output.txt", "UTF-8")
    try {
      writer.println(this)
    } catch {
      case e @ (_: FileNotFoundException | _: UnsupportedEncodingException) => e.printStackTrace()
    } finally {
      writer.close();
    }
  }

  override def toString(): String = {
    println("State.toString printing")
    toStringHelper(0, "")
  }

  override def compareTo(o: Any): Int = ???

  private def toStringHelper(d: Int, ind: String): String = {
    var str = ind + player + " to play\n"
    str = str + ind + "Value: " + value + "\n"
    str = str + board.toString(ind) + "\n"
    if (children != null && children.length > 0) {
      str = str + ind + "Children at depth " + (d + 1) + ":\n" + ind +
        "----------------\n"
      for (s <- children) {
        str = str + s.toStringHelper(d + 1, ind + "   ")
      }
    }
    str
  }
}

object State {

  val length0 = Array[State]()
}

