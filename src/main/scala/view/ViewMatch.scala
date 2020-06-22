package view

import java.awt.{Dimension, GridBagConstraints, GridBagLayout}

import javax.swing._
import model.{GameSnapshot, Piece, Player, Snapshot}
import utils.BoardGame.{Board, BoardCell}
import utils.Coordinate

import scala.collection.mutable

trait ViewMatch {

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
    * Shows the specified snapshot of the game
    *
    * @param gameSnapshot
    *                 snapshot to show.
    */
  def update(gameSnapshot: GameSnapshot)

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

}

object ViewMatch {
  def apply(gameView: ViewHnefatafl): ViewMatch = ViewMatchImpl(gameView)

  case class ViewMatchImpl(view: ViewHnefatafl) extends ViewMatch {

    private var gamePanel, northPanel, subNorthPanel, southPanel, subSouthPanel, boardPanel, boardPlusColumns,
        leftPanel, rightPanel: JPanel = _

    private var playerOrWinnerLabel: JLabel = _
    private val playerWhiteLabel: JLabel = MenuFactory.createLabelWhitePlayer
    private val playerBlackLabel: JLabel = MenuFactory.createLabelBlackPlayer
    private var menuButton, firstMoveButton, nextMoveButton, previousMoveButton, lastMoveButton, undoMoveButton: JButton = _
    private val cells: mutable.HashMap[Coordinate, Cell] = mutable.HashMap.empty
    private var possibleMoves: Seq[Coordinate] = Seq.empty
    private var selectedCell: Option[Coordinate] = Option.empty
    private var board: Board = _
    private var player: Player.Val = view.getMenuUtils.getPlayer
    private var lastMoveCells: Option[(Cell, Cell)] = Option.empty
    private var kingCoordinate: Option[Coordinate] = Option.empty

    override def initGamePanel(board: Board): JPanel = {
      this.board = board

      GameFactory.setVariantBoardSize(this.view.getDimension)

      initNorthPanel()
      initSouthPanel()
      initLeftRightPanel()
      initBoard()
      drawPawns(board.cells)

      gamePanel = GameFactory.createGamePanel
      gamePanel.add(northPanel)

      boardPlusColumns = GameFactory.createBoardPlusColumnsPanel
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

    override def update(gameSnapshot: GameSnapshot): Unit = {
      player = gameSnapshot.getPlayerToMove
      addLostPawns(gameSnapshot.getNumberCapturedBlacks, gameSnapshot.getNumberCapturedWhites)
      drawPawns(gameSnapshot.getBoard.cells)

      possibleMoves.foreach(c => cells(c).unsetAsPossibleMove())
      deselectCell()

      if(kingCoordinate.nonEmpty)
        cells(kingCoordinate.get).resetKingCell()

      if(!gameSnapshot.getWinner.equals(Player.None))
        kingCoordinate = Option(view.findKing())

      setStatusGame(gameSnapshot.getWinner)
      resetLastMoveCells()
      if(gameSnapshot.getLastMove.nonEmpty)
        highlightLastMove(gameSnapshot.getLastMove.get)

      boardPanel.repaint()
      boardPanel.validate()
      gamePanel.validate()

      /* TODO NON FUNZIONA */
      //ViewFactory.generateASoundForMove()
    }

    override def activeNextLast(): Unit = {
      nextMoveButton.setEnabled(true)
      lastMoveButton.setEnabled(true)
    }

    override def disableNextLast(): Unit = {
      nextMoveButton.setEnabled(false)
      lastMoveButton.setEnabled(false)
      undoMoveButton.setEnabled(true)
    }

    override def activeFirstPrevious(): Unit = {
      previousMoveButton.setEnabled(true)
      firstMoveButton.setEnabled(true)
    }

    override def disableFirstPrevious(): Unit = {
      previousMoveButton.setEnabled(false)
      firstMoveButton.setEnabled(false)
    }

    override def activeUndo(): Unit = undoMoveButton.setEnabled(true)

    override def disableUndo(): Unit = undoMoveButton.setEnabled(false)

    /**
      * Sets the end game.
      *
      * @param winner
      *                 winner of the game.
      */
    private def setStatusGame(winner: Player.Val): Unit = winner match {
      case Player.White =>
        playerOrWinnerLabel.setForeground(ColorProvider.getWhiteColor)
        playerOrWinnerLabel.setText("White has Won!")
        playerWhiteLabel.setVisible(true)
        playerBlackLabel.setVisible(false)
        cells(kingCoordinate.get).setAsKingEscapedCell()

      case Player.Black =>
        playerOrWinnerLabel.setForeground(ColorProvider.getBlackColor)
        playerOrWinnerLabel.setText("Black has Won!")
        playerWhiteLabel.setVisible(false)
        playerBlackLabel.setVisible(true)
        cells(kingCoordinate.get).setAsKingCapturedCell()

      case Player.Draw => playerOrWinnerLabel.setText("Draw!")

      case _ => switchPlayerLabel()
    }

    /**
      * Switches the player label showed.
      */
    private def switchPlayerLabel(): Unit = {
      playerOrWinnerLabel.setText(player + " moves")
      if(player.equals(Player.White)) {
        playerBlackLabel.setVisible(false)
        playerWhiteLabel.setVisible(true)
      } else {
        playerBlackLabel.setVisible(true)
        playerWhiteLabel.setVisible(false)
      }
    }

    /**
      * Highlights the cells where the last move occured.
      *
      * @param lastMove
      *                 last move fromCoordinate and toCoordinate
      */
    private def highlightLastMove(lastMove: (Coordinate, Coordinate)): Unit = {
      lastMoveCells = Option(cells(lastMove._1), cells(lastMove._2))
      lastMoveCells.get._1.setAsLastMoveCell()
      lastMoveCells.get._2.setAsLastMoveCell()
    }

    /**
      * Resets the cells of the last move.
      */
    private def resetLastMoveCells(): Unit = {
      if(lastMoveCells.nonEmpty) {
        lastMoveCells.get._1.unsetAsLastMoveCell()
        lastMoveCells.get._2.unsetAsLastMoveCell()
      }
    }

    /**
      * Initializes the board panel.
      */
    private def initBoard(): Unit = {
      boardPanel = GameFactory.createBoardPanel
      val layout: GridBagLayout = new java.awt.GridBagLayout()
      boardPanel.setLayout(layout)
      val lim: GridBagConstraints = new java.awt.GridBagConstraints()
      for (i <- 1 to view.getDimension) {
        for (j <- 1 to view.getDimension) {
          val coordinate: Coordinate = Coordinate(i, j)
          val cell: Cell = setTypeCell(coordinate)
          cell.addActionListener(_ => actionCell(cell))
          lim.gridx = j
          lim.gridy = i
          layout.setConstraints(cell, lim)
          cells += coordinate -> cell
          boardPanel.add(cell)
        }
      }
    }

    /**
      * Initializes the north panel.
      */
    private def initNorthPanel(): Unit = {
      northPanel = GameFactory.createTopBottomPanel
      northPanel.add(Box.createRigidArea(new Dimension(GameFactory.getSmallerSide * 10/100,GameFactory.getSmallerSide * 8 / 100)))
      subNorthPanel = GameFactory.createGameSubMenuPanel
      val lim: GridBagConstraints = new java.awt.GridBagConstraints()
      lim.gridx = 0
      lim.gridy = 0
      lim.weightx = 0
      lim.fill = GridBagConstraints.NONE
      lim.anchor = GridBagConstraints.LINE_START

      playerBlackLabel.setVisible(true)
      playerWhiteLabel.setVisible(false)
      subNorthPanel.add(playerBlackLabel, lim)
      subNorthPanel.add(playerWhiteLabel, lim)

      playerOrWinnerLabel = GameFactory.createLabelPlayerToMoveWinner
      lim.gridx = 1
      subNorthPanel.add(playerOrWinnerLabel, lim)
      northPanel.add(subNorthPanel)
      northPanel.add(Box.createRigidArea(new Dimension(GameFactory.getSmallerSide * 55/100,GameFactory.getSmallerSide * 8 / 100)))
      menuButton = GameFactory.createGameButton()
      menuButton.addActionListener(_ => view.switchOverlay(gamePanel, view.getInGameMenuPanel))
      northPanel.add(menuButton)
      northPanel.add(Box.createRigidArea(new Dimension(GameFactory.getSmallerSide * 20/100,GameFactory.getSmallerSide * 8 / 100)))
    }

    /**
      * Initializes the south panel.
      */
    private def initSouthPanel(): Unit = {
      southPanel = GameFactory.createTopBottomPanel
      southPanel.add(Box.createRigidArea(new Dimension(GameFactory.getSmallerSide * 30/100, GameFactory.getSmallerSide * 8 / 100)))
      subSouthPanel = GameFactory.createGameSubMenuPanel
      val lim: GridBagConstraints = new java.awt.GridBagConstraints()
      lim.gridy = 0
      lim.weightx = 1
      lim.fill = GridBagConstraints.NONE
      lim.anchor = GridBagConstraints.CENTER

      firstMoveButton = GameFactory.createFirstMoveButton()
      firstMoveButton.addActionListener(_ => changeSnapshot(Snapshot.First))
      lim.gridx = 0
      subSouthPanel.add(firstMoveButton, lim)

      previousMoveButton = GameFactory.createPreviousMoveButton()
      previousMoveButton.addActionListener(_ => changeSnapshot(Snapshot.Previous))
      lim.gridx = 1
      subSouthPanel.add(previousMoveButton, lim)

      nextMoveButton = GameFactory.createNextMoveButton()
      nextMoveButton.addActionListener(_ => changeSnapshot(Snapshot.Next))
      lim.gridx = 2
      subSouthPanel.add(nextMoveButton, lim)

      lastMoveButton = GameFactory.createLastMoveButton()
      lastMoveButton.addActionListener(_ => changeSnapshot(Snapshot.Last))
      lim.gridx = 3
      subSouthPanel.add(lastMoveButton, lim)

      undoMoveButton = GameFactory.createUndoMoveButton()
      undoMoveButton.addActionListener(_ => undoMove())
      lim.gridx = 4
      subSouthPanel.add(undoMoveButton, lim)
      southPanel.add(subSouthPanel)
      southPanel.add(Box.createRigidArea(new Dimension(GameFactory.getSmallerSide * 10/100,GameFactory.getSmallerSide * 8 / 100)))
    }

    /**
      * Initializes the panels of captures.
      */
    private def initLeftRightPanel(): Unit = {
      leftPanel = GameFactory.createLeftRightPanel(1, view.getDimension)
      rightPanel = GameFactory.createLeftRightPanel(1, view.getDimension)
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
      val coordinateStart: Coordinate = selectedCell.get
      val coordinateArrival: Coordinate = getCoordinate(cell)
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
    private def makeMove(fromCoordinate: Coordinate, toCoordinate: Coordinate): Unit = {
      view.makeMove(fromCoordinate, toCoordinate)
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
      drawLostPawns(Player.Black, nBlackCaptured)
      drawLostPawns(Player.White, nWhiteCaptured)
    }

    /**
      * Draws the number of captured pieces white or black.
      *
      * @param player
      *                 player to move.
      * @param length
      *                 number of captured pieces.
      */
    private def drawLostPawns(player: Player.Val, length: Int): Unit = {
      val panel: JPanel = if (Player.Black eq player) leftPanel else rightPanel
      panel.removeAll()
      for (_ <- 0 until length) {
        panel.add(createLostPawn(player))
      }
      panel.repaint()
    }

    /**
      * Adds the captured piece white or black.
      *
      * @return label
      */
    private def createLostPawn(player: Player.Val): JLabel = player match {
      case Player.Black => GameFactory.createLostBlackPawn
      case _ => GameFactory.createLostWhitePawn
    }

    /**
      * Gets the coordinate of a specifies cell.
      *
      * @param cell
      *               specifies cell.
      *
      * @return pair of coordinate
      */
    private def getCoordinate(cell: JButton): Coordinate = {
      cells.filter(v => v._2.equals(cell)).head._1
    }

    /**
      * Requires for a move.
      *
      * @param coordinate
      *               specifies coordinate.
      */
    private def moveRequest(coordinate: Coordinate): Unit = {
      possibleMoves = view.getPossibleMoves(coordinate)
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
      val piece: Piece.Val = cell.getPiece
      val button: JButton = cells(cell.getCoordinate)
      piece match {
        case Piece.WhitePawn => button.add(GameFactory.createWhitePawn)
        case Piece.BlackPawn => button.add(GameFactory.createBlackPawn)
        case Piece.WhiteKing => button.add(GameFactory.createWhiteKing)
        case _ =>
      }
    }

    /**
     * Sets the type of cell.
     *
     * @param cell
     *             cell to be set.
     */
    private def setTypeCell(cell: Coordinate): Cell = cell match {
      case coordinate if view.isCornerCell(coordinate) => GameFactory.createCornerCell()
      case coordinate if view.isCentralCell(coordinate) => GameFactory.createCenterCell()
      case coordinate if view.isPawnCell(coordinate) => GameFactory.createPawnCell()
      case _ => GameFactory.createNormalCell()

    }

    /**
      * Show next, previous, first or last move.
      *
      * @param snapshotToShow
      *             snapshot to show.
      */
    private def changeSnapshot(snapshotToShow: Snapshot.Value): Unit = view.changeSnapshot(snapshotToShow)

    /**
      * Delete last move.
      */
    private def undoMove(): Unit = view.undoMove()
  }
}
