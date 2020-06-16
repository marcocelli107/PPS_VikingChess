package view

import controller.ControllerHnefatafl
import javax.swing.{JFrame, JPanel}
import model.Player
import utils.BoardGame.Board
import utils.Pair

trait GameView {
  def getDimension: Int
  def showOverlay(actualPanel: JPanel, panelToShow: JPanel)
  def getInGameMenuPanel: JPanel
  def makeMove(fromCoordinate: Pair[Int], toCoordinate: Pair[Int])
  def getPossibleMoves(coordinate: Pair[Int]): Seq[Pair[Int]]

  def updateMove(playerToMove: Player.Value, winner: Player.Value, board: Board, nBlackCaptured: Int, nWhiteCaptured: Int)
  def setEndGame(winner: Player.Value, kingCoordinate: Option[Pair[Int]])

  def initOrRestoreGUI()

  def getGamePanel: JPanel
  def getMenuUtils: Menu

  /**
   * Checks if the cell at the specified coordinate is the central cell.
   *
   * @param coordinate
   *                   coordinate of the cell to inspect
   *
   * @return boolean.
   */
  def isCentralCell(coordinate: Pair[Int]): Boolean

  /**
   * Checks if the cell at the specified coordinate is a corner cell.
   *
   * @param coordinate
   *                   coordinate of the cell to inspect
   *
   * @return boolean.
   */
  def isCornerCell(coordinate: Pair[Int]): Boolean
}

object GameView {
  def apply(controller: ControllerHnefatafl): GameView = GameViewImpl(controller)

  case class GameViewImpl(controller: ControllerHnefatafl) extends GameView {
    private val menuUtils: Menu = Menu(this)
    private val gameUtils: Game = Game(this)
    private val frame: JFrame = ViewFactory.createFrame
    private val overlayPanel: JPanel = ViewFactory.createOverlayLayoutPanel
    private var gamePanel: JPanel = ViewFactory.createGamePanel
    private var menuPanel, variantsPanel, diffPanel, inGameMenuPanel, playerChoicePanel: JPanel = _

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

    private var dimension: Int = _

    private var board: Board = _

    frame.add(overlayPanel)
    frame.setVisible(true)


    def getInGameMenuPanel: JPanel = inGameMenuPanel

    def getGamePanel: JPanel = gamePanel

    def getDimension: Int = dimension

    override def getMenuUtils: Menu = menuUtils

    private def initMainMenu(): Unit = {
      menuPanel = menuUtils.initMenu
    }

    private def initVariantsMenu(): Unit = {
      variantsPanel = menuUtils.initVariantsMenu
    }

    private def initDiffMenu(): Unit = {
      diffPanel = menuUtils.initDiffMenu
    }

    private def initPlayerChoiceMenu(): Unit = {
      playerChoicePanel = menuUtils.initPlayerChoiceMenu
    }

    private def initInGameMenu(): Unit = {
      inGameMenuPanel = menuUtils.initInGameMenu
    }

    private def initGamePanel(board: Board): Unit = {
      gamePanel = gameUtils.initGamePanel(board)
    }

    def initOrRestoreGUI(): Unit = {
      if (gamePanel.getComponents.length > 0) {
        gameUtils.restoreGame()
        overlayPanel.remove(gamePanel)
      }
      val newGame: (Board, Player.Value) = controller.newGame(menuUtils.getBoardVariant)
      board = newGame._1.asInstanceOf[Board]
      dimension = board.size
      initGamePanel(board)
      overlayPanel.add(gamePanel)
      showGame()
      gameUtils.getLabelPlayer.setText(newGame._2 + " moves.")
    }

    private def showGame(): Unit = {
      playerChoicePanel.setVisible(false)
      gamePanel.setVisible(true)
    }

    def showOverlay(actualPanel: JPanel, panelToShow: JPanel): Unit = {
      actualPanel.setVisible(false)
      panelToShow.setVisible(true)
    }

    def getPossibleMoves(coordinate: Pair[Int]): Seq[Pair[Int]] = {
      controller.getPossibleMoves(coordinate)
    }

    def makeMove(fromCoordinate: Pair[Int], toCoordinate: Pair[Int]): Unit = {
      controller.makeMove(fromCoordinate, toCoordinate)
    }

    def updateMove(playerToMove: Player.Value, winner: Player.Value, board: Board, nBlackCaptured: Int, nWhiteCaptured: Int): Unit = {
      gameUtils.updateMove(playerToMove, winner, board, nBlackCaptured, nWhiteCaptured)
      gameUtils.highlightLastMove(controller.getLastMove)
    }

    def setEndGame(winner: Player.Value, kingCoordinate: Option[Pair[Int]]): Unit = {
      gameUtils.setEndGame(winner, kingCoordinate)
    }

    override def isCentralCell(coordinate: Pair[Int]): Boolean = controller.isCentralCell(coordinate)

    override def isCornerCell(coordinate: Pair[Int]): Boolean = controller.isCornerCell(coordinate)
  }
}
