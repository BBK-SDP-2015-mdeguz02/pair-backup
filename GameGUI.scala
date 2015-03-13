package connectfour

object GameGUI extends App {
  /* -------------------------- Change these to play game differently. -------------------------- */

  /* p1 is the first player, p2 is the second player. A Solver
         * can be a Human, AI, or Dummy. Human and Dummy constructors have
         * a player parameter; the AI constructor has a player and depth
         * as parameters, with the a depth used to recurse when searching the
         * game space. */

  val p1 = new AI(RED, 3);
  val p2 = new Human(YELLOW);

  val b = new Board()
  
  val move = new Move(RED, 1)
  b.makeMove(move)
  val move2 = new Move(YELLOW, 1)
  b.makeMove(move2)
  val move3 = new Move(RED, 1)
  b.makeMove(move3)
  val move4 = new Move(YELLOW, 1)
  b.makeMove(move4)
  val move5 = new Move(RED, 2)
  b.makeMove(move5)
  val move6 = new Move(YELLOW, 2)
  b.makeMove(move6)
  val move7 = new Move(RED, 3)
  b.makeMove(move7)
  val move8 = new Move(RED, 3)
  b.makeMove(move8)
  
  val currentState = new State(YELLOW, b, move8)
  // currentState.initializeChildren()
  
  AI.createGameTree(currentState, 3) // create tree to depth 3
  currentState.writeToFile() // SUCCESS!
  
  /* --------------------------------- Do not change below here. --------------------------------- */

  
  //val game = new Game(p1, p2);
  //game.setGUI(new GUI(game, Board.NUM_COLS, Board.NUM_ROWS));
  //game.runGame();
}