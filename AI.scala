package connectfour
import scala.collection.mutable.ArrayBuffer

class AI(private var player: Player, private var depth: Int) extends Solver {

  // Remember that the AI is player, who wants max value
  
  
  override def getMoves(b: Board): Array[Move] = {
    
    val currentState = new State(player, b, null)
    AI.createGameTree(currentState, depth)    
    minimax(currentState)
    
    currentState.writeToFile() // debugging
    
    val movetemp = Array(new Move(player, 5))
    movetemp
    
    // ask classmates! - this code isn't working (GRRRRR!)
    // we want to filter out values that 
    //val tempChildren = currentState.getChildren()
    //tempChildren.filter(child => child.getValue == currentStateValue)
    //            .foreach(state => state.getLastMove)   
    //})
  }

  
  // referenced Minimax pseudocode on Wikipedia
  def minimax(s: State): Unit = {
   
    def getMax(s: State): Int = {
    // if a leaf node, give the node a score
      if (s.getChildren().length == 0) {
        s.setValue(evaluateBoard(s.getBoard))
        s.getValue        
      } else {
      
        var bestValue = Int.MinValue
        var tempChildren = s.getChildren() 
        tempChildren.foreach(child => {
          val childBestValue = getMin(child)
          if (childBestValue > bestValue)
            bestValue = childBestValue 
        })
        s.setValue(bestValue)
        bestValue
      }
    }
    
    def getMin(s: State): Int = {
      
      // if a leaf node, give the node a score
      if (s.getChildren().length == 0) {
        s.setValue(evaluateBoard(s.getBoard))
        s.getValue        
      } else {
      
        var bestValue = Int.MaxValue
      
        var tempChildren = s.getChildren()
        tempChildren.foreach(child => {
          val childBestValue = getMax(child)
          if (childBestValue < bestValue)
            bestValue = childBestValue
        })
        s.setValue(bestValue)
        bestValue
      }
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
  def createGameTree(s: State, d: Int): Unit = {
    if (d != 0) {
      s.initializeChildren()
      val tempChildren = s.getChildren()
      tempChildren.foreach(child => createGameTree(child, d-1))
    }
  }
   
  def minimax(ai: AI, s: State) {
 
    ai.minimax(s)
  }
}

