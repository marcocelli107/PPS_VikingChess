package view

import controller.ControllerHnefatafl
import javax.swing.{JFrame, JPanel}
import model.{GameSnapshot, Player, Snapshot}
import utils.BoardGame.Board
import utils.Coordinate

trait ViewHnefatafl {

  /**
    * Gets the dimension according to game variant.
    */
  def getDimension: Int

  /**
    * Switches panel in overlay.
    *
    * @param actualPanel
    *                 panel shown.
    * @param panelToShow
    *                 panel to show.
    */
  def switchOverlay(actualPanel: JPanel, panelToShow: JPanel)

  /**
    * Gets the game menù.
    *
    * @return game menù panel.
    */
  def getInGameMenuPanel: JPanel

  /**
    * Gets the game panel.
    *
    * @return game panel.
    */
  def getGamePanel: JPanel

  /**
    * Gets the menù.
    *
    * @return menù panel.
    */
  def getMenuUtils: Menu

  /**
    * Initializes or restores the game.
    */
  def initOrRestoreGUI()

  /**
    * Makes move.
    *
    * @param fromCoordinate
    *              starting coordinate.
    * @param toCoordinate
    *              arrival coordiante.
    */
  def makeMove(fromCoordinate: Coordinate, toCoordinate: Coordinate)

  /**
    * Gets the possible moves from a specifies coordiante.
    *
    * @param coordinate
    *              starting coordinate.
    * @return sequence of possible coordinates.
    */
  def getPossibleMoves(coordinate: Coordinate): Seq[Coordinate]

  /**
    * Sets the move made by the user.
    *
    * @param gameSnapshot
    *                 snapshot to show.
    */
  def update(gameSnapshot: GameSnapshot)

  /**
   * Checks if the cell at the specified coordinate is the central cell.
   *
   * @param coordinate
   *                   coordinate of the cell to inspect
   *
   * @return boolean.
   */
  def isCentralCell(coordinate: Coordinate): Boolean

  /**
   * Checks if the cell at the specified coordinate is a corner cell.
   *
   * @param coordinate
   *                   coordinate of the cell to inspect
   *
   * @return boolean.
   */
  def isCornerCell(coordinate: Coordinate): Boolean

  /**
    * Checks if the cell at the specified coordinate is a init pawn cell.
    *
    * @param coordinate
    *                   coordinate of the cell to inspect
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
    *                   indicates snapshot to show.
    */
  def changeSnapshot(snapshotToShow: Snapshot.Value): Unit

  /**
   * Undoes last move.
   */
  def undoMove(): Unit
}

object ViewHnefatafl {

  def apply(controller: ControllerHnefatafl): ViewHnefatafl = ViewHnefataflImpl(controller)

  case class ViewHnefataflImpl(controller: ControllerHnefatafl) extends ViewHnefatafl {

    private var menuPanel, variantsPanel, diffPanel, inGameMenuPanel, playerChoicePanel: JPanel = _
    private var dimension: Int = _
    private var board: Board = _
    private val viewMainMenu: Menu = Menu(this)
    private val viewMatch: ViewMatch = ViewMatch(this)
    private val frame: JFrame = ViewFactory.createFrame
    private val overlayPanel: JPanel = ViewFactory.createOverlayLayoutPanel
    private var gamePanel: JPanel = ViewFactory.createGamePanel

    initMainMenu()
    overlayPanel.add(menuPanel)
    initVariantsMenu()
    overlayPanel.add(variantsPanel)
    initDiffMenu()
    overlayPanel.add(diffPanel)
    initPlayerChoiceMenu()
    overlayPanel.add(playerChoicePanel)
    initInGameMenu()
    overlayPanel.add(inGameMenuPanel)

    frame.add(overlayPanel)
    frame.setVisible(true)

    override def getDimension: Int = dimension

    override def switchOverlay(actualPanel: JPanel, panelToShow: JPanel): Unit = {
      actualPanel.setVisible(false)
      panelToShow.setVisible(true)
    }

    override def getInGameMenuPanel: JPanel = inGameMenuPanel

    override def getGamePanel: JPanel = gamePanel

    override def getMenuUtils: Menu = viewMainMenu

    override def initOrRestoreGUI(): Unit = {
      if (gamePanel.getComponents.length > 0) {
        viewMatch.restoreGame()
        overlayPanel.remove(gamePanel)
      }
      val newGame: (Board, Player.Value) = controller.newGame(viewMainMenu.getBoardVariant,viewMainMenu.getGameMode, viewMainMenu.getDifficult)
      board = newGame._1.asInstanceOf[Board]
      dimension = board.size
      initGamePanel(board)
      overlayPanel.add(gamePanel)
      showGame()
      viewMatch.getLabelPlayer.setText(newGame._2 + " moves.")
    }

    override def makeMove(fromCoordinate: Coordinate, toCoordinate: Coordinate): Unit = {
      controller.makeMove(fromCoordinate, toCoordinate)
    }

    override def getPossibleMoves(coordinate: Coordinate): Seq[Coordinate] = {
      controller.getPossibleMoves(coordinate)
    }

    override def update(gameSnapshot: GameSnapshot): Unit = viewMatch.update(gameSnapshot)

    override def isCentralCell(coordinate: Coordinate): Boolean = controller.isCentralCell(coordinate)

    override def isCornerCell(coordinate: Coordinate): Boolean = controller.isCornerCell(coordinate)

    override def isPawnCell(coordinate: Coordinate): Boolean = controller.isPawnCell(coordinate)

    override def findKing(): Coordinate = controller.findKing()

    override def changeSnapshot(snapshotToShow: Snapshot.Value): Unit = controller.changeSnapshot(snapshotToShow)

    override def undoMove(): Unit = controller.undoMove()

    /**
      * Initializes the main menù.
      */
    private def initMainMenu(): Unit = {
      menuPanel = viewMainMenu.initMenu
    }

    /**
      * Initializes the variant menù.
      */
    private def initVariantsMenu(): Unit = {
      variantsPanel = viewMainMenu.initVariantsMenu
    }

    /**
      * Initializes the difficult selection menù.
      */
    private def initDiffMenu(): Unit = {
      diffPanel = viewMainMenu.initDiffMenu
    }

    /**
      * Initializes the player selection menù.
      */
    private def initPlayerChoiceMenu(): Unit = {
      playerChoicePanel = viewMainMenu.initPlayerChoiceMenu
    }

    /**
      * Initializes the game menù.
      */
    private def initInGameMenu(): Unit = {
      inGameMenuPanel = viewMainMenu.initInGameMenu
    }

    /**
      * Initializes the game panel.
      *
      * @param board
      *               board returned from parser.
      */
    private def initGamePanel(board: Board): Unit = {
      gamePanel = viewMatch.initGamePanel(board)
    }

    /**
      * Show the game panel.
      */
    private def showGame(): Unit = {
      playerChoicePanel.setVisible(false)
      gamePanel.setVisible(true)
    }
  }
}
