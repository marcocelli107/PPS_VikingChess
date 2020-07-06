package view

import controller.ControllerHnefatafl
import javax.swing.{JFrame, JPanel}
import model.GameMode.GameMode
import model.Player.Player
import model.Snapshot.Snapshot
import model.GameSnapshot
import utils.{Coordinate, Move}

trait ViewHnefatafl {

  /**
    * Calls controller to get the dimension according to game variant.
    *
    * @return dimension
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
  def initOrRestoreGUI(playerChosen: Player)

  /**
    * Makes move.
    *
    * @param move
   *             move to make
    */
  def makeMove(move: Move)

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
    * Notifies the viewer a change snapshot to view.
    *
    * @param gameSnapshot
    *                 snapshot to show.
    */
  def changeSnapshot(gameSnapshot: GameSnapshot): Unit

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
  def changeSnapshot(snapshotToShow: Snapshot): Unit

  /**
   * Undoes last move.
   */
  def undoMove(): Unit

  /**
    * Actives/Disables next and last move.
    */
  def activeNextLast()
  def disableNextLast()

  /**
    * Actives/Disables previous and first move.
    */
  def activeFirstPrevious()
  def disableFirstPrevious()

  /**
    * Actives/Disables undo move.
    */
  def activeUndo()
  def disableUndo()

  /**
    * Gets player chosen from user.
    */
  def getPlayerChosen: Player

  /**
    * Gets game mode chosen from user.
    */
  def getGameMode: GameMode
}

object ViewHnefatafl {

  def apply(controller: ControllerHnefatafl): ViewHnefatafl = ViewHnefataflImpl(controller)

  case class ViewHnefataflImpl(controller: ControllerHnefatafl) extends ViewHnefatafl {

    private var menuPanel, variantsPanel, difficultyPanel, inGameMenuPanel, playerChoicePanel: JPanel = _
    private var dimension: Int = _
    private val viewMainMenu: Menu = Menu(this)
    private val viewMatch: ViewMatch = ViewMatch(this)
    private val frame: JFrame = GameFactory.createFrame
    private val overlayPanel: JPanel = GameFactory.createOverlayLayoutPanel
    private var gamePanel: JPanel = GameFactory.createGamePanel

    menuPanel = viewMainMenu.initMenu
    overlayPanel.add(menuPanel)
    variantsPanel = viewMainMenu.initVariantsMenu
    overlayPanel.add(variantsPanel)
    difficultyPanel = viewMainMenu.initDiffMenu
    overlayPanel.add(difficultyPanel)
    playerChoicePanel = viewMainMenu.initPlayerChoiceMenu
    overlayPanel.add(playerChoicePanel)
    inGameMenuPanel = viewMainMenu.initInGameMenu
    overlayPanel.add(inGameMenuPanel)

    frame.add(overlayPanel)
    frame.setVisible(true)

    override def getDimension: Int = controller.getDimension

    override def switchOverlay(actualPanel: JPanel, panelToShow: JPanel): Unit = {
      actualPanel.setVisible(false)
      panelToShow.setVisible(true)
    }

    override def getInGameMenuPanel: JPanel = inGameMenuPanel

    override def getGamePanel: JPanel = gamePanel

    override def getMenuUtils: Menu = viewMainMenu

    override def initOrRestoreGUI(playerChosen: Player): Unit = {
      if (gamePanel.getComponents.length > 0) {
        viewMatch.restoreGame()
        overlayPanel.remove(gamePanel)
      }
      val newGame: GameSnapshot = controller.newGame(viewMainMenu.getBoardVariant, viewMainMenu.getGameMode, viewMainMenu.getDifficulty, viewMainMenu.getPlayer)
      dimension = newGame.getBoard.size
      gamePanel = viewMatch.initGamePanel()
      overlayPanel.add(gamePanel)
      viewMatch.update(newGame)
      showGame()
      controller.startGame()
    }

    override def makeMove(move: Move): Unit = controller.makeMove(move)

    override def getPossibleMoves(coordinate: Coordinate): Seq[Coordinate] = controller.getPossibleMoves(coordinate)

    override def update(gameSnapshot: GameSnapshot): Unit = viewMatch.update(gameSnapshot)

    override def changeSnapshot(gameSnapshot: GameSnapshot): Unit = viewMatch.updateSnapshot(gameSnapshot)

    override def isCentralCell(coordinate: Coordinate): Boolean = controller.isCentralCell(coordinate)

    override def isCornerCell(coordinate: Coordinate): Boolean = controller.isCornerCell(coordinate)

    override def isPawnCell(coordinate: Coordinate): Boolean = controller.isPawnCell(coordinate)

    override def findKing(): Coordinate = controller.findKing()

    override def changeSnapshot(snapshotToShow: Snapshot): Unit = controller.changeSnapshot(snapshotToShow)

    override def undoMove(): Unit = controller.undoMove()

    override def disableNextLast(): Unit = viewMatch.disableNextLast()

    override def disableFirstPrevious(): Unit = viewMatch.disableFirstPrevious()

    override def activeUndo(): Unit = viewMatch.activeUndo()

    override def disableUndo(): Unit = viewMatch.disableUndo()

    override def activeNextLast(): Unit = viewMatch.activeNextLast()

    override def activeFirstPrevious(): Unit = viewMatch.activeFirstPrevious()

    override def getPlayerChosen: Player = viewMainMenu.getPlayer

    override def getGameMode: GameMode = viewMainMenu.getGameMode

    /**
      * Show the game panel.
      */
    private def showGame(): Unit = {
      playerChoicePanel.setVisible(false)
      gamePanel.setVisible(true)
    }
  }
}
