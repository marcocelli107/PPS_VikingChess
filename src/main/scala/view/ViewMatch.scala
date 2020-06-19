package view

import java.awt.{GridBagConstraints, GridBagLayout}

import javax.swing.{JButton, JLabel, JPanel}
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
    * Sets the end game.
    *
    * @param winner
    *                 winner of the game.
    */
  def setEndGame(winner: Player.Val)
}

object ViewMatch {
  def apply(gameView: ViewHnefatafl): ViewMatch = ViewMatchImpl(gameView)

  case class ViewMatchImpl(view: ViewHnefatafl) extends ViewMatch {

    private var gamePanel, northPanel, southPanel, boardPanel, boardPlusColumns,
        leftPanel, rightPanel: JPanel = _

    private var playerOrWinnerLabel: JLabel = _
    private val playerWhiteLabel: JLabel = ViewFactory.createLabelWhitePlayer
    private val playerBlackLabel: JLabel = ViewFactory.createLabelBlackPlayer
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

      ViewFactory.setVariantBoardSize(this.view.getDimension)

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

    override def update(gameSnapshot: GameSnapshot): Unit = {
      player = gameSnapshot.getPlayerToMove
      addLostPawns(gameSnapshot.getNumberCapturedBlacks, gameSnapshot.getNumberCapturedWhites)
      drawPawns(gameSnapshot.getBoard.cells)

      possibleMoves.foreach(c => cells(c).unsetAsPossibleMove())
      deselectCell()

      if(kingCoordinate.nonEmpty)
        cells(kingCoordinate.get).resetKingCell()

      if(gameSnapshot.getWinner.equals(Player.None))
        switchPlayerLabel()
      else {
        kingCoordinate = Option(view.findKing())
        setEndGame(gameSnapshot.getWinner)
      }

      resetLastMoveCells()
      if(gameSnapshot.getLastMove.nonEmpty)
        highlightLastMove(gameSnapshot.getLastMove.get)


      boardPanel.repaint()
      boardPanel.validate()
      gamePanel.validate()
    }

    override def setEndGame(winner: Player.Val): Unit = winner match {
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
      boardPanel = ViewFactory.createBoardPanel
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
      northPanel = ViewFactory.createTopBottomPanel
      val layout: GridBagLayout = new java.awt.GridBagLayout()
      northPanel.setLayout(layout)
      val lim: GridBagConstraints = new java.awt.GridBagConstraints()
      menuButton = ViewFactory.createGameButton()
      menuButton.addActionListener(_ => view.switchOverlay(gamePanel, view.getInGameMenuPanel))
      lim.gridx = 0
      lim.gridy = 0
      lim.weightx = 0
      lim.fill = GridBagConstraints.NONE
      lim.anchor = GridBagConstraints.CENTER
      playerBlackLabel.setVisible(true)
      playerWhiteLabel.setVisible(false)
      northPanel.add(playerBlackLabel, lim)
      northPanel.add(playerWhiteLabel, lim)

      playerOrWinnerLabel = ViewFactory.createLabelPlayerToMoveWinner
      lim.gridx = 1
      northPanel.add(playerOrWinnerLabel, lim)

      menuButton = ViewFactory.createGameButton()
      menuButton.addActionListener(_ => view.switchOverlay(gamePanel, view.getInGameMenuPanel))
      lim.gridx = 2
      lim.anchor = GridBagConstraints.LINE_END
      northPanel.add(menuButton, lim)
    }

    /**
      * Initializes the south panel.
      */
    private def initSouthPanel(): Unit = {
      southPanel = ViewFactory.createTopBottomPanel
      val layout: GridBagLayout = new java.awt.GridBagLayout()
      southPanel.setLayout(layout)
      val lim: GridBagConstraints = new java.awt.GridBagConstraints()
      lim.gridy = 0
      lim.weightx = 1
      lim.fill = GridBagConstraints.NONE
      lim.anchor = GridBagConstraints.CENTER

      firstMoveButton = ViewFactory.createFirstMoveButton()
      firstMoveButton.addActionListener(_ => changeSnapshot(Snapshot.First))
      lim.gridx = 0
      southPanel.add(firstMoveButton, lim)

      previousMoveButton = ViewFactory.createPreviousMoveButton()
      previousMoveButton.addActionListener(_ => changeSnapshot(Snapshot.Previous))
      lim.gridx = 1
      southPanel.add(previousMoveButton, lim)

      nextMoveButton = ViewFactory.createNextMoveButton()
      nextMoveButton.addActionListener(_ => changeSnapshot(Snapshot.Next))
      lim.gridx = 2
      southPanel.add(nextMoveButton, lim)

      lastMoveButton = ViewFactory.createLastMoveButton()
      lastMoveButton.addActionListener(_ => changeSnapshot(Snapshot.Last))
      lim.gridx = 3
      southPanel.add(lastMoveButton, lim)

      undoMoveButton = ViewFactory.createUndoMoveButton()
      undoMoveButton.addActionListener(_ => undoMove())
      lim.gridx = 4
      southPanel.add(undoMoveButton, lim)
    }

    /**
      * Initializes the panels of captures.
      */
    private def initLeftRightPanel(): Unit = {
      leftPanel = ViewFactory.createLeftRightPanel(1, view.getDimension)
      rightPanel = ViewFactory.createLeftRightPanel(1, view.getDimension)
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
    private def setTypeCell(cell: Coordinate): Cell = cell match {
      case coordinate if view.isCornerCell(coordinate) => ViewFactory.createCornerCell()
      case coordinate if view.isCentralCell(coordinate) => ViewFactory.createCenterCell()
      case coordinate if view.isPawnCell(coordinate) => ViewFactory.createPawnCell()
      case _ => ViewFactory.createNormalCell()

    }

    private def changeSnapshot(snapshotToShow: Snapshot.Value): Unit = view.changeSnapshot(snapshotToShow)

    private def undoMove(): Unit = view.undoMove()
  }
}
