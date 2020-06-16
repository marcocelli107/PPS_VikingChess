package view

import java.awt.{GridBagConstraints, GridBagLayout}

import javax.swing.{JButton, JLabel, JPanel}
import model.{Piece, Player}
import utils.BoardGame.{Board, BoardCell}
import utils.Pair

import scala.collection.mutable

trait Game {
  def initGamePanel(board: Board): JPanel
  def restoreGame()
  def getLabelPlayer: JLabel
  def updateMove(playerToMove: Player.Value, winner: Player.Value, currentBoard: Board, nBlackCaptured: Int, nWhiteCaptured: Int)
  def setEndGame(winner: Player.Value, kingCoordinate: Option[Pair[Int]])
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
    private val player: Player.Value = gameView.getMenuUtils.getPlayer
    private var lastMoveCells: Option[(Cell, Cell)] = Option.empty

    def initGamePanel(board: Board): JPanel = {
      this.board = board

      ViewFactory.setVariantBoardSize(this.gameView.getDimension)

      initNorthPanel()
      initSouthPanel()
      initLeftRightPanel()
      initBoard()
      setPawns(board.cells)

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

    private def initBoard(): Unit = {
      boardPanel = ViewFactory.createBoardPanel
      val layout: GridBagLayout = new java.awt.GridBagLayout()
      boardPanel.setLayout(layout)
      val lim: GridBagConstraints = new java.awt.GridBagConstraints()
      for (i <- 1 to gameView.getDimension) {
        for (j <- 1 to gameView.getDimension) {
          val coordinate: Pair[Int] = Pair(i, j)
          val cell: Cell = cellChoice(coordinate)
          cell.addActionListener(_ => actionCell(cell))
          lim.gridx = j
          lim.gridy = i
          layout.setConstraints(cell, lim)
          cells.put(coordinate, cell)
          boardPanel.add(cell)
        }
      }
    }

    private def initNorthPanel(): Unit = {
      northPanel = ViewFactory.createTopBottomPanel
      val layout: GridBagLayout = new java.awt.GridBagLayout()
      northPanel.setLayout(layout)
      val lim: GridBagConstraints = new java.awt.GridBagConstraints()
      menuButton = ViewFactory.createGameButton("")
      menuButton.addActionListener(_ => gameView.showOverlay(gamePanel, gameView.getInGameMenuPanel))
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

    private def initSouthPanel(): Unit = {
      southPanel = ViewFactory.createTopBottomPanel
    }

    private def initLeftRightPanel(): Unit = {
      leftPanel = ViewFactory.createLeftRightPanel(1, gameView.getDimension)
      rightPanel = ViewFactory.createLeftRightPanel(1, gameView.getDimension)
    }

    private def actionCell(cell: JButton): Unit = {
      if (cell.getComponents.length > 0 && possibleMoves.isEmpty)
        actionSelectCell(cell)
      else if (possibleMoves.nonEmpty && !possibleMoves.contains(getCoordinate(cell)))
        actionDeselectCell()
      else if (possibleMoves.contains(getCoordinate(cell)) && selectedCell.isDefined)
        actionMovePawn(cell)
    }

    private def actionSelectCell(cell: JButton): Unit = {
      selectedCell = Option(getCoordinate(cell))
      moveRequest(getCoordinate(cell))
    }

    private def actionDeselectCell(): Unit = {
      possibleMoves.foreach(c => cells(c).unsetAsPossibleMove())
      deselectCell()
    }

    private def actionMovePawn(cell: JButton): Unit = {
      val coordinateStart: Pair[Int] = selectedCell.get
      val coordinateArrival: Pair[Int] = getCoordinate(cell)
      moveAndPaint(coordinateStart, coordinateArrival)
    }

    private def moveAndPaint(fromCoordinate: Pair[Int], toCoordinate: Pair[Int]): Unit = {
      gameView.makeMove(fromCoordinate, toCoordinate)
    }

    private def addLostPawns(nBlackCaptured: Int, nWhiteCaptured: Int): Unit = {
      val length: Int = if (player eq Player.Black) nBlackCaptured else nWhiteCaptured
      val panel: JPanel = if (Player.Black eq player) leftPanel else rightPanel
      panel.removeAll()
      for (_ <- 0 until length) {
        panel.add(createLostPawn)
      }
      panel.repaint()
    }

    private def createLostPawn: JLabel = player match {
      case Player.Black => ViewFactory.createLostBlackPawn
      case _ => ViewFactory.createLostWhitePawn
    }

    private def getCoordinate(cell: JButton): Pair[Int] = {
      cells.filter(v => v._2.equals(cell)).head._1
    }

    private def moveRequest(coordinate: Pair[Int]): Unit = {
      possibleMoves = gameView.getPossibleMoves(coordinate)
      possibleMoves.foreach(c => cells(c).setAsPossibleMove())
    }

    private def deselectCell(): Unit = {
      selectedCell = Option.empty
      possibleMoves = Seq.empty
    }

    private def setPawns(positions: Seq[BoardCell]): Unit = {
      for (p <- positions) {
        val button: JButton = cells(p.getCoordinate)
        if (button.getComponentCount > 0) button.removeAll()
        pawnChoice(p)
      }
    }

    private def pawnChoice(c: BoardCell): Unit = {
      val piece: Piece.Value = c.getPiece
      val button: JButton = cells(c.getCoordinate)
      piece match {
        case Piece.WhitePawn => button.add(ViewFactory.createWhitePawn)
        case Piece.BlackPawn => button.add(ViewFactory.createBlackPawn)
        case Piece.WhiteKing => button.add(ViewFactory.createWhiteKing)
        case _ =>
      }
    }

    override def updateMove(playerToMove: Player.Value, winner: Player.Value, currentBoard: Board, nBlackCaptured: Int, nWhiteCaptured: Int): Unit = {
      addLostPawns(nBlackCaptured, nWhiteCaptured)
      setPawns(currentBoard.cells)
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

    private def cellChoice(c: Pair[Int]): Cell = {
      if (gameView.isCornerCell(c))
        return ViewFactory.createCornerCell()
      if (gameView.isCentralCell(c))
        return ViewFactory.createCenterCell()
      ViewFactory.createNormalCell()
    }

    override def restoreGame(): Unit = {
      cells.clear()
      gamePanel.removeAll()
    }

    override def getLabelPlayer: JLabel = playerOrWinnerLabel

    def highlightLastMove(lastMove: (Pair[Int], Pair[Int])): Unit = {
      if(lastMoveCells.nonEmpty) {
        lastMoveCells.get._1.unsetAsLastMoveCell()
        lastMoveCells.get._2.unsetAsLastMoveCell()
      }
      lastMoveCells = Option(cells(lastMove._1), cells(lastMove._2))
      lastMoveCells.get._1.setAsLastMoveCell()
      lastMoveCells.get._2.setAsLastMoveCell()
    }

  }
}
