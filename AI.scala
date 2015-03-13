package connectfour
import scala.collection.mutable.ArrayBuffer

class AI(private var player: Player, private var depth: Int) extends Solver {

  override def getMoves(b: Board): Array[Move] = { null }

  // referenced Minimax pseudocode on Wikipedia
  def minimax(s: State): Int = {
    
    // Remember that the AI is player, who wants max value

    if (s.getChildren() == null) { // This is a leaf node
      s.value = evaluateBoard(s.getBoard()) // returns s.value
    } 
    
    if (s.getPlayer() == player) { // This is the maximising player
      var bestValue = Int.MinValue
      
      var tempChildren = s.getChildren() 
      tempChildren.foreach(child => {
        val childBestValue = minimax(child)
        if (childBestValue > bestValue)
          bestValue = childBestValue 
      })
      bestValue
    }
    
    else { // player is opponent
      var bestValue = Int.MaxValue
      
      var tempChildren = s.getChildren()
      tempChildren.foreach(child => {
        val childBestValue = minimax(child)
        if (childBestValue < bestValue)
          bestValue = childBestValue
      })
      bestValue
    }
  }

  def evaluateBoard(b: Board): Int = {
    val winner = b.hasConnectFour()
    var value = 0
    if (!winner.isDefined) {
      val locs = b.winLocations()
      for (loc <- locs; p <- loc) {
        value += (if (p == player) 1 else if (p != null) -1 else 0)
      }
    } else {
      var numEmpty = 0
      var r = 0
      while (r < Board.NUM_ROWS) {
        var c = 0
        while (c < Board.NUM_COLS) {
          if (b.getTile(r, c) == null) numEmpty += 1
          c = c + 1
        }
        r = r + 1
      }
      value = (if (winner.get == player) 1 else -1) * 10000 * numEmpty
    }
    value
  }
}

object AI {

  /**
   * Generate the game tree with root s of depth d.
   * The game tree's nodes are State objects that represent the state of a game
   * and whose children are all possible States that can result from the next move.
   *
   * NOTE: this method runs in exponential time with respect to d.
   * With d around 5 or 6, it is extremely slow and will start to take a very
   * long time to run.
   *
   * Note: If s has a winner (four in a row), it should be a leaf.
   */
  def createGameTree(s: State, d: Int): Boolean = {
    if (d != 0) {
      s.initializeChildren()
      val tempChildren = s.getChildren()
      tempChildren.foreach(child => createGameTree(child, d-1))
      true
    }
    false // Why does CreateGameTree need a return type?
  }
   
  def minimax(ai: AI, s: State) {
    ai.minimax(s)
  }
}

