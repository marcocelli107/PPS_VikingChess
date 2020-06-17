package view

import java.awt.{GridBagConstraints, GridBagLayout}

import javax.swing.{JButton, JLabel, JPanel}
import model.{Piece, Player}
import utils.BoardGame.{Board, BoardCell}
import utils.Pair

import scala.collection.mutable

trait Game {

  /**
    * Initializes the game panel.
    *
    * @param board
*                 board returned from parser.
    *
    * @return panel
    */
  def initGamePanel(board: Board): JPanel

  /**
    * Restores the game.
    */
  def restoreGame()

  /**
    * Gets the label of the player to move or of the winner.
    *
    * @return label
    */
  def getLabelPlayer: JLabel

  /**
    * Sets the move made by the user.
    *
    * @param playerToMove
    *                 next player to move.
    * @param winner
    *                 possible winner.
    * @param currentBoard
    *                 board updated after the move.
    * @param nBlackCaptured
    *                 total number of black pieces captured.
    * @param nWhiteCaptured
    *                 total number of white pieces captured.
    */
  def updateMove(playerToMove: Player.Value, winner: Player.Value, currentBoard: Board, nBlackCaptured: Int, nWhiteCaptured: Int)

  /**
    * Sets the end game.
    *
    * @param winner
    *                 winner of the game.
    * @param kingCoordinate
    *                 king's coordinate in the board.
    */
  def setEndGame(winner: Player.Value, kingCoordinate: Option[Pair[Int]])

  /**
   * Highlights the cells where the last move occured.
   *
   * @param lastMove
   *                 last move fromCoordinate and toCoordinate
   */
  def highlightLastMove(lastMove: (Pair[Int], Pair[Int]))
}

object Game {
  def apply(gameView: GameView): Game = GameImpl(gameView)

  case class GameImpl(gameView: GameView) extends Game {

    private var gamePanel, northPanel, southPanel, boardPanel, boardPlusColumns,
        leftPanel, rightPanel: JPanel = _

    private var playerOrWinnerLabel: JLabel = _
    private var menuButton: JButton = _
    private val cells: mutable.HashMap[Pair[Int], Cell] = mutable.HashMap.empty
    private var possibleMoves: Seq[Pair[Int]] = Seq.empty
    private var selectedCell: Option[Pair[Int]] = Option.empty
    private var board: Board = _
    private var player: Player.Value = gameView.getMenuUtils.getPlayer
    private var lastMoveCells: Option[(Cell, Cell)] = Option.empty

    override def initGamePanel(board: Board): JPanel = {
      this.board = board

      ViewFactory.setVariantBoardSize(this.gameView.getDimension)

      initNorthPanel()
      initSouthPanel()
      initLeftRightPanel()
      initBoard()
      drawPawns(board.cells)

      gamePanel = ViewFactory.createGamePanel
      gamePanel.add(northPanel)

      boardPlusColumns = ViewFactory.createBoardPlusColumnsPanel
      boardPlusColumns.add(leftPanel)
      boardPlusColumns.add(boardPanel)
      boardPlusColumns.add(rightPanel)

      gamePanel.add(boardPlusColumns)
      gamePanel.add(southPanel)
      gamePanel
    }

    override def restoreGame(): Unit = {
      cells.clear()
      gamePanel.removeAll()
    }

    override def getLabelPlayer: JLabel = playerOrWinnerLabel

    override def updateMove(playerToMove: Player.Value, winner: Player.Value, currentBoard: Board, nBlackCaptured: Int, nWhiteCaptured: Int): Unit = {
      player = playerToMove
      addLostPawns(nBlackCaptured, nWhiteCaptured)
      drawPawns(currentBoard.cells)
      boardPanel.repaint()
      possibleMoves.foreach(c => cells(c).unsetAsPossibleMove())
      deselectCell()
      boardPanel.validate()
      gamePanel.validate()

      if(winner.equals(Player.None))
        playerOrWinnerLabel.setText(playerToMove + " moves")
    }

    override def setEndGame(winner: Player.Value, kingCoordinate: Option[Pair[Int]]): Unit = (winner, kingCoordinate) match {
      case (Player.White, _) =>
        playerOrWinnerLabel.setForeground(ColorProvider.getWhiteColor)
        playerOrWinnerLabel.setText("White has Won!")
        cells(kingCoordinate.get).setAsKingEscapedCell()

      case (Player.Black, _) =>
        playerOrWinnerLabel.setForeground(ColorProvider.getBlackColor)
        playerOrWinnerLabel.setText("Black has Won!")
        cells(kingCoordinate.get).setAsKingCapturedCell()

      case (Player.Draw, _) => playerOrWinnerLabel.setText("Draw!")
    }

    override def highlightLastMove(lastMove: (Pair[Int], Pair[Int])): Unit = {
      if(lastMoveCells.nonEmpty) {
        lastMoveCells.get._1.unsetAsLastMoveCell()
        lastMoveCells.get._2.unsetAsLastMoveCell()
      }
      lastMoveCells = Option(cells(lastMove._1), cells(lastMove._2))
      lastMoveCells.get._1.setAsLastMoveCell()
      lastMoveCells.get._2.setAsLastMoveCell()
    }

    /**
      * Initializes the board panel.
      */
    private def initBoard(): Unit = {
      boardPanel = ViewFactory.createBoardPanel
      val layout: GridBagLayout = new java.awt.GridBagLayout()
      boardPanel.setLayout(layout)
      val lim: GridBagConstraints = new java.awt.GridBagConstraints()
      for (i <- 1 to gameView.getDimension) {
        for (j <- 1 to gameView.getDimension) {
          val coordinate: Pair[Int] = Pair(i, j)
          val cell: Cell = setTypeCell(coordinate)
          cell.addActionListener(_ => actionCell(cell))
          lim.gridx = j
          lim.gridy = i
          layout.setConstraints(cell, lim)
          cells.put(coordinate, cell)
          boardPanel.add(cell)
        }
      }
    }

    /**
      * Initializes the north panel.
      */
    private def initNorthPanel(): Unit = {
      northPanel = ViewFactory.createTopBottomPanel
      val layout: GridBagLayout = new java.awt.GridBagLayout()
      northPanel.setLayout(layout)
      val lim: GridBagConstraints = new java.awt.GridBagConstraints()
      menuButton = ViewFactory.createGameButton()
      menuButton.addActionListener(_ => gameView.switchOverlay(gamePanel, gameView.getInGameMenuPanel))
      lim.gridx = 0
      lim.gridy = 0
      lim.weightx = 1
      lim.fill = GridBagConstraints.NONE
      lim.anchor = GridBagConstraints.LINE_END
      northPanel.add(menuButton, lim)
      playerOrWinnerLabel = ViewFactory.createLabelPlayerToMoveWinner
      lim.anchor = GridBagConstraints.CENTER
      northPanel.add(playerOrWinnerLabel, lim)
    }

    /**
      * Initializes the south panel.
      */
    private def initSouthPanel(): Unit = {
      southPanel = ViewFactory.createTopBottomPanel
      val layout: GridBagLayout = new java.awt.GridBagLayout()
      southPanel.setLayout(layout)
      val lim: GridBagConstraints = new java.awt.GridBagConstraints()
      menuButton = ViewFactory.createPreviousMoveButton()
      //menuButton.addActionListener(_ => gameView.switchOverlay(gamePanel, gameView.getInGameMenuPanel))
      lim.gridx = 0
      lim.gridy = 0
      lim.weightx = 1
      lim.fill = GridBagConstraints.NONE
      lim.anchor = GridBagConstraints.CENTER
      southPanel.add(menuButton, lim)

      menuButton = ViewFactory.createNextMoveButton()
      //menuButton.addActionListener(_ => gameView.switchOverlay(gamePanel, gameView.getInGameMenuPanel))
      lim.gridx = 1
      lim.gridy = 0
      lim.weightx = 1
      lim.fill = GridBagConstraints.NONE
      lim.anchor = GridBagConstraints.CENTER
      southPanel.add(menuButton, lim)

      menuButton = ViewFactory.createUndoMoveButton()
      //menuButton.addActionListener(_ => gameView.switchOverlay(gamePanel, gameView.getInGameMenuPanel))
      lim.gridx = 2
      lim.gridy = 0
      lim.weightx = 1
      lim.fill = GridBagConstraints.NONE
      lim.anchor = GridBagConstraints.CENTER
      southPanel.add(menuButton, lim)
    }

    /**
      * Initializes the panels of captures.
      */
    private def initLeftRightPanel(): Unit = {
      leftPanel = ViewFactory.createLeftRightPanel(1, gameView.getDimension)
      rightPanel = ViewFactory.createLeftRightPanel(1, gameView.getDimension)
    }

    /**
      * Defines the action to show the possible moves from a specifies cell.
      *
      * @param cell
      *               cell where possible moves begin.
      */
    private def actionCell(cell: JButton): Unit = {
      if(cell.getComponents.length > 0 && possibleMoves.isEmpty)
        actionSelectCell(cell)
      else if(possibleMoves.nonEmpty && !possibleMoves.contains(getCoordinate(cell))) {
        actionDeselectCell()
        actionSelectCell(cell)
      }
      else if(possibleMoves.contains(getCoordinate(cell)) && selectedCell.isDefined)
        actionMovePawn(cell)
      else
        actionDeselectCell()
    }

    /**
      * Defines the selection of a cell.
      *
      * @param cell
      *               starting cell.
      */
    private def actionSelectCell(cell: JButton): Unit = {
      selectedCell = Option(getCoordinate(cell))
      moveRequest(getCoordinate(cell))
    }

    /**
      * Defines the deselection of a cell.
      */
    private def actionDeselectCell(): Unit = {
      possibleMoves.foreach(c => cells(c).unsetAsPossibleMove())
      deselectCell()
    }

    /**
      * Defines the movement action from a cell to another cell.
      *
      * @param cell
      *               arrival cell.
      */
    private def actionMovePawn(cell: JButton): Unit = {
      val coordinateStart: Pair[Int] = selectedCell.get
      val coordinateArrival: Pair[Int] = getCoordinate(cell)
      makeMove(coordinateStart, coordinateArrival)
    }

    /**
      * Calls game view for make the move.
      *
      * @param fromCoordinate
      *                 starting coordinate.
      * @param toCoordinate
      *                 arrival coordinate.
      */
    private def makeMove(fromCoordinate: Pair[Int], toCoordinate: Pair[Int]): Unit = {
      gameView.makeMove(fromCoordinate, toCoordinate)
    }

    /**
      * Updates the number of captured pieces white or black.
      *
      * @param nBlackCaptured
      *                 number of captured pieces black.
      * @param nWhiteCaptured
      *                 number of captured pieces white.
      */
    private def addLostPawns(nBlackCaptured: Int, nWhiteCaptured: Int): Unit = {
      val length: Int = if (player eq Player.Black) nBlackCaptured else nWhiteCaptured
      val panel: JPanel = if (Player.Black eq player) leftPanel else rightPanel
      panel.removeAll()
      for (_ <- 0 until length) {
        panel.add(createLostPawn)
      }
      panel.repaint()
    }

    /**
      * Adds the captured piece white or black.
      *
      * @return label
      */
    private def createLostPawn: JLabel = player match {
      case Player.Black => ViewFactory.createLostBlackPawn
      case _ => ViewFactory.createLostWhitePawn
    }

    /**
      * Gets the coordinate of a specifies cell.
      *
      * @param cell
      *               specifies cell.
      *
      * @return pair of coordinate
      */
    private def getCoordinate(cell: JButton): Pair[Int] = {
      cells.filter(v => v._2.equals(cell)).head._1
    }

    /**
      * Requires for a move.
      *
      * @param coordinate
      *               specifies coordinate.
      */
    private def moveRequest(coordinate: Pair[Int]): Unit = {
      possibleMoves = gameView.getPossibleMoves(coordinate)
      possibleMoves.foreach(c => cells(c).setAsPossibleMove())
      if(possibleMoves.nonEmpty)
        cells(selectedCell.get).setAsSelectedCell()
    }

    /**
      * Empties the selected cell and its possible moves.
      */
    private def deselectCell(): Unit = {
      if(selectedCell.nonEmpty)
        cells(selectedCell.get).unsetAsSelectedCell()
      selectedCell = Option.empty
      possibleMoves = Seq.empty
    }

    /**
      * Sets pieces in the board.
      *
      * @param positions
      *             positions sequence of the cell.
      */
    private def drawPawns(positions: Seq[BoardCell]): Unit = {
      for (p <- positions) {
        val button: JButton = cells(p.getCoordinate)
        if (button.getComponentCount > 0) button.removeAll()
        pawnChoice(p)
      }
    }

    /**
      * Sets the type of piece.
      *
      * @param cell
      *             cell to be set.
      */
    private def pawnChoice(cell: BoardCell): Unit = {
      val piece: Piece.Value = cell.getPiece
      val button: JButton = cells(cell.getCoordinate)
      piece match {
        case Piece.WhitePawn => button.add(ViewFactory.createWhitePawn)
        case Piece.BlackPawn => button.add(ViewFactory.createBlackPawn)
        case Piece.WhiteKing => button.add(ViewFactory.createWhiteKing)
        case _ =>
      }
    }

    /**
     * Sets the type of cell.
     *
     * @param cell
     *             cell to be set.
     */
    private def setTypeCell(cell: Pair[Int]): Cell = cell match {
      case coordinate if gameView.isCornerCell(coordinate) => ViewFactory.createCornerCell()
      case coordinate if gameView.isCentralCell(coordinate) => ViewFactory.createCenterCell()
      case coordinate if gameView.isPawnCell(coordinate) => ViewFactory.createPawnCell()
      case _ => ViewFactory.createNormalCell()

    }
  }
}
