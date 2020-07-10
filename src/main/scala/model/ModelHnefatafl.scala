package model

import akka.actor.{ActorRef, ActorSystem, Props}
import controller.ControllerHnefatafl
import model.game.GameMode.GameMode
import model.game.GameSnapshot.GameSnapshotImpl
import model.game.GameVariant.GameVariant
import model.game.Level.Level
import model.game.Player.Player
import model.game.Snapshot.Snapshot
import model.game.BoardGame.Board
import ia.minimax.{ArtificialIntelligenceImpl, FindBestMoveMsg}
import ia.pruning_alpha_beta.MiniMax
import model.game.{Coordinate, GameMode, GameSnapshot, Move, Player, Snapshot}
import model.prolog.ParserProlog

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait ModelHnefatafl {

  /**
    * Gets dimension of board from a specified version.
    *
    * @return dimension
    */
  def getDimension: Int

  /**
   * Calls parser for a new Game.
   *
   * @return game snapshot
   */
  def createGame(): GameSnapshot

  /**
    * Initializes IA in PVE mode. IA makes first move if is your turn.
    */
  def startGame(): Unit

  /**
   * Calls parser for the possible moves from a cell.
   *
   * @param cell
   *                  coordinate of the Cell.
   *
   * @return list buffer of the possible computed moves.
   */
  def showPossibleCells(cell: Coordinate): Seq[Coordinate]

  /**
    * Called by actor IA for best move.
    */
  def iaBestMove(move: Move): Unit

  /**
   * Calls parser for making a move from coordinate to coordinate.
   *
   * @param move
   *                    move to make
   *
   * @return updated board.
   */
  def makeMove(move: Move): Unit

  /**
   * Checks if the cell at the specified coordinate is the central cell.
   *
   * @param coordinate
   *                      coordinate of the cell to inspect
   *
   * @return boolean.
   */
  def isCentralCell(coordinate: Coordinate): Boolean

  /**
   * Checks if the cell at the specified coordinate is a corner cell.
   *
   * @param coordinate
   *                        coordinate of the cell to inspect
   *
   * @return boolean.
   */
  def isCornerCell(coordinate: Coordinate): Boolean

  /**
   * Checks if the cell at the specified coordinate is a init pawn cell.
   *
   * @param coordinate
   *                        coordinate of the cell to inspect
   *
   * @return boolean.
   */
  def isPawnCell(coordinate: Coordinate): Boolean

  /**
   * Find king coordinate in the current board.
   *
   * @return king coordinate to list.
   */
  def findKing(): Coordinate

  /**
   * Returns a previous or later state of the current board.
   *
   * @param snapshotToShow
   *                        indicates snapshot to show.
   *
   * @return required board
   */
  def changeSnapshot(snapshotToShow: Snapshot): Unit

  /**
   * Undoes last move.
   */
  def undoMove(): Unit
}

object ModelHnefatafl {

  def apply(controller: ControllerHnefatafl, newVariant: GameVariant, gameMode: GameMode, levelIA: Level, playerChosen: Player): ModelHnefatafl = ModelHnefataflImpl(controller, newVariant, gameMode, levelIA, playerChosen)

  case class ModelHnefataflImpl(controller: ControllerHnefatafl, newVariant: GameVariant, gameMode: GameMode, level: Level, playerChosen: Player) extends ModelHnefatafl {

    private var storySnapshot: mutable.ListBuffer[GameSnapshot] = _
    private var currentSnapshot: Int = 0
    private val moveLogPrint: Boolean = false
    private var system: ActorSystem = _
    private var refIA: ActorRef = _
    private var iaSnapshot: GameSnapshot = _

    /**
     * Defines status of the current game.
     */
    private var game: (Player, Player, Board, Int) = _

    private final val SIZE_DRAW: Int = 9

    /**
      * Defines the game variant.
      */
    private val currentVariant: GameVariant = newVariant

    /**
      * Defines the chosen mode.
      */
    private val mode: GameMode = gameMode

    /**
      * Defines the chosen level of IA.
      */
    private val levelIA: Level = level

    override def getDimension: Int = newVariant.boardSize

    override def createGame(): GameSnapshot = {

      game = ParserProlog.createGame(currentVariant.toString.toLowerCase)
      storySnapshot = mutable.ListBuffer(GameSnapshotImpl(currentVariant, game._1, game._2, game._3, Option.empty, 0, 0))
      storySnapshot.last
    }

    override def startGame(): Unit = {
      if(mode.equals(GameMode.PVE)) {
        initIA()
        if(iaTurn)
          makeMoveIA()
      }
    }

    override def showPossibleCells(cell: Coordinate): Seq[Coordinate] = {
      if(!iaTurn)
        if (showingCurrentSnapshot)
          ParserProlog.showPossibleCells(cell)
        else ListBuffer.empty
      else
        ListBuffer.empty
    }

    override def makeMove(move: Move): Unit = {
      if(moveLogPrint)
        println("snapshot = MoveGenerator.makeMove(snapshot, Move(Coordinate(" + move.from.x + "," + move.from.y + "), " + "Coordinate(" + move.to.x + "," + move.to.y + ")))")

      game = ParserProlog.makeLegitMove(move)

      val pieceCaptured: (Int, Int) = incrementCapturedPieces(game._1, game._4)
      var winner: Player = game._2

      if (checkThreefoldRepetition())
        winner = Player.Draw

      storySnapshot += GameSnapshot(currentVariant, game._1, winner, game._3, Option(move), pieceCaptured._1, pieceCaptured._2)

      currentSnapshot += 1

      controller.activeFirstPrevious()
      controller.activeUndo()

      controller.updateView(storySnapshot.last)

      if(mode.equals(GameMode.PVE) && storySnapshot.last.getWinner.equals(Player.None) && iaTurn)
        makeMoveIA()
    }

    override def iaBestMove(move: Move): Unit = {
      if(notUndone)
        makeMove(move)
    }

    override def isCentralCell(coordinate: Coordinate): Boolean = ParserProlog.isCentralCell(coordinate)

    override def isCornerCell(coordinate: Coordinate): Boolean = ParserProlog.isCornerCell(coordinate)

    override def isPawnCell(coordinate: Coordinate): Boolean = ParserProlog.isPawnCell(coordinate)

    override def findKing(): Coordinate = ParserProlog.findKing()

    override def changeSnapshot(previousOrNext: Snapshot): Unit = {
      previousOrNext match {
        case Snapshot.Previous => decrementCurrentSnapshot()
        case Snapshot.Next => incrementCurrentSnapshot()
        case Snapshot.First => currentSnapshot = 0; controller.disableFirstPrevious(); controller.activeNextLast()
        case Snapshot.Last => currentSnapshot = storySnapshot.size - 1; controller.disableNextLast(); controller.activeFirstPrevious()
      }
      val gameSnapshot = storySnapshot(currentSnapshot)
      controller.changeSnapshotView(gameSnapshot)
    }

    override def undoMove(): Unit = {
      if (showingCurrentSnapshot & storySnapshot.last.getLastMove.nonEmpty) {
        storySnapshot -= storySnapshot.last
        currentSnapshot -= 1
        controller.activeFirstPrevious()
        ParserProlog.undoMove(storySnapshot.last.getBoard)
        controller.changeSnapshotView(storySnapshot.last)
      }
      if(storySnapshot.size == 1) {
        controller.disableNextLast()
        controller.disableFirstPrevious()
        controller.disableUndo()
      }
      if(mode.equals(GameMode.PVE)) {
        if(iaTurn)
          makeMoveIA()
      }
    }

    private def notUndone: Boolean = iaSnapshot.equals(storySnapshot.last)

    /**
      * Sends a messages to IA actor for make a move.
      */
    private def makeMoveIA(): Unit = {
      //SEQUENTIAL PRUNING ALPHA-BETA
      //makeMove(sequIA.findBestMove(storySnapshot.last))

      //MINIMAX ACTORS
      iaSnapshot = storySnapshot.last
      refIA = system.actorOf(Props(ArtificialIntelligenceImpl(this, levelIA)))
      refIA ! FindBestMoveMsg(storySnapshot.last)
    }

    /**
     * Increments the number of pieces captured of the player.
     */
    private def incrementCapturedPieces(player: Player, piecesCaptured: Int): (Int, Int) = player match {
      case Player.Black => (storySnapshot.last.getNumberCapturedBlacks + piecesCaptured, storySnapshot.last.getNumberCapturedWhites)
      case Player.White => (storySnapshot.last.getNumberCapturedBlacks, storySnapshot.last.getNumberCapturedWhites + piecesCaptured)
      case _ => null
    }

    /**
     * Checks if there was a threefold repetition.
     *
     * @return boolean
     */
    private def checkThreefoldRepetition(): Boolean = storySnapshot.reverse.take(SIZE_DRAW) match {
      case l if l.isEmpty || l.size < SIZE_DRAW => false
      case l if l.head.equals(l(4)) && l(4).equals(l(8)) => true
      case _ => false
    }

    /**
      * Increments current snapshot.
      */
    private def incrementCurrentSnapshot(): Unit = {
      if(!showingCurrentSnapshot) {
        currentSnapshot += 1
        controller.activeFirstPrevious()
      }
      if(showingCurrentSnapshot) controller.disableNextLast()
    }

    /**
      * Decrements current snapshot.
      */
    private def decrementCurrentSnapshot(): Unit = {
      if(currentSnapshot > 0) {
        currentSnapshot -= 1
        controller.activeNextLast()
      }
      if(currentSnapshot == 0) controller.disableFirstPrevious()
    }

    /**
      * Checks if the currentSnapshot is the last.
      */
    private def showingCurrentSnapshot: Boolean = currentSnapshot == storySnapshot.size - 1

    /**
      * Actives the IA Actor
      */
    private def initIA(): Unit = {
      system = ActorSystem()
      //sequIA = MiniMaxImpl(levelIA)
    }

    /**
      * Checks if IA turn.
      *
      * @return boolean
      */
    private def iaTurn: Boolean = !(storySnapshot.last.getPlayerToMove.equals(playerChosen) || playerChosen.equals(Player.None))
  }
}