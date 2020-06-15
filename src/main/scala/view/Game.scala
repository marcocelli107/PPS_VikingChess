package view

import java.awt.{Color, GridBagConstraints, GridBagLayout}

import javax.swing.{JButton, JLabel, JPanel}
import model.{Piece, Player}
import utils.BoardGame.{Board, BoardCell}
import utils.Pair
import view.ColorProvider.ColorProviderImpl

import scala.collection.mutable

trait Game {
  def initGamePanel(board: Board): JPanel
  def restoreGame()
  def getLabelPlayer: JLabel
  def updateMove(playerToMove: Player.Value, winner: Player.Value, currentBoard: Board, nBlackCaptured: Int, nWhiteCaptured: Int)
  def setEndGame(winner: Player.Value, kingCoordinate: Option[Pair[Int]])

}

object Game {
  def apply(viewFactory: ViewFactory, gameView: GameView): Game = GameImpl(viewFactory, gameView)

  case class GameImpl(viewFactory: ViewFactory, gameView: GameView) extends Game {
    private var gamePanel, northPanel, southPanel, boardPanel, boardPlusColumns,
        leftPanel, rightPanel: JPanel = _

    private var playerOrWinnerLabel: JLabel = _
    private var menuButton: JButton = _
    private val cells: mutable.HashMap[Pair[Int], JButton] = mutable.HashMap.empty
    private var possibleMoves: Seq[Pair[Int]] = Seq.empty
    private var selectedCell: Option[Pair[Int]] = Option.empty
    private var board: Board = _
    private val colorProvider: ColorProvider = new ColorProviderImpl
    private val player: Player.Value = gameView.getMenuUtils.getPlayer

    def initGamePanel(board: Board): JPanel = {
      this.board = board

      initNorthPanel()
      initSouthPanel()
      initLeftRightPanel()
      initBoard()
      setPawns(board.cells)

      gamePanel = viewFactory.createGamePanel
      gamePanel.add(northPanel)

      boardPlusColumns = viewFactory.createBoardPlusColumnsPanel
      boardPlusColumns.add(leftPanel)
      boardPlusColumns.add(boardPanel)
      boardPlusColumns.add(rightPanel)

      gamePanel.add(boardPlusColumns)
      gamePanel.add(southPanel)
      gamePanel
    }

    private def initBoard(): Unit = {
      boardPanel = viewFactory.createBoardPanel
      val layout: GridBagLayout = new java.awt.GridBagLayout()
      boardPanel.setLayout(layout)
      val lim: GridBagConstraints = new java.awt.GridBagConstraints()
      for (i <- 1 to gameView.getDimension) {
        for (j <- 1 to gameView.getDimension) {
          val coordinate: Pair[Int] = Pair(i, j)
          val cell: JButton = cellChoice(coordinate)
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
      northPanel = viewFactory.createTopBottomPanel
      val layout: GridBagLayout = new java.awt.GridBagLayout()
      northPanel.setLayout(layout)
      val lim: GridBagConstraints = new java.awt.GridBagConstraints()
      menuButton = viewFactory.createGameButton("")
      menuButton.addActionListener(_ => gameView.showOverlay(gamePanel, gameView.getInGameMenuPanel))
      lim.gridx = 0
      lim.gridy = 0
      lim.weightx = 1
      lim.fill = GridBagConstraints.NONE
      lim.anchor = GridBagConstraints.LINE_END
      northPanel.add(menuButton, lim)

      playerOrWinnerLabel = viewFactory.createLabelPlayerToMoveWinner
      lim.anchor = GridBagConstraints.CENTER
      northPanel.add(playerOrWinnerLabel, lim)
    }

    private def initSouthPanel(): Unit = {
      southPanel = viewFactory.createTopBottomPanel
    }

    private def initLeftRightPanel(): Unit = {
      leftPanel = viewFactory.createLeftRightPanel(1, gameView.getDimension)
      rightPanel = viewFactory.createLeftRightPanel(1, gameView.getDimension)
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
      possibleMoves.foreach(c => setColorBackground(c, colorProvider.getNormalCellColor))
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
      case Player.Black => viewFactory.createLostBlackPawn
      case _ => viewFactory.createLostWhitePawn
    }

    private def getCoordinate(cell: JButton): Pair[Int] = {
      cells.filter(v => v._2.equals(cell)).head._1
    }

    private def moveRequest(coordinate: Pair[Int]): Unit = {
      possibleMoves = gameView.getPossibleMoves(coordinate)
      possibleMoves.foreach(k => cells(k).setBackground(colorProvider.getPossibleMovesColor))
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
        case Piece.WhitePawn => button.add(viewFactory.createWhitePawn)
        case Piece.BlackPawn => button.add(viewFactory.createBlackPawn)
        case Piece.WhiteKing => button.add(viewFactory.createWhiteKing)
        case _ =>
      }
    }

    override def updateMove(playerToMove: Player.Value, winner: Player.Value, currentBoard: Board, nBlackCaptured: Int, nWhiteCaptured: Int): Unit = {
      addLostPawns(nBlackCaptured, nWhiteCaptured)
      setPawns(currentBoard.cells)
      boardPanel.repaint()
      possibleMoves.foreach(c => setColorBackground(c, colorProvider.getNormalCellColor))
      deselectCell()
      boardPanel.validate()
      gamePanel.validate()

      if(winner.equals(Player.None))
        playerOrWinnerLabel.setText(playerToMove + " moves")
    }

    override def setEndGame(winner: Player.Value, kingCoordinate: Option[Pair[Int]]): Unit = (winner, kingCoordinate) match {
      case (Player.White, _) =>
        playerOrWinnerLabel.setForeground(colorProvider.getWhiteColor)
        playerOrWinnerLabel.setText("White has Won!")
        cells(kingCoordinate.get).setBackground(colorProvider.getWhiteWinColor)

      case (Player.Black, _) =>
        playerOrWinnerLabel.setForeground(colorProvider.getBlackColor)
        playerOrWinnerLabel.setText("Black has Won!")
        cells(kingCoordinate.get).setBackground(colorProvider.getBlackWinColor)

      case (Player.Draw, _) => playerOrWinnerLabel.setText("Draw!")
    }

    private def cellChoice(c: Pair[Int]): JButton = {
      if (gameView.isCornerCell(c))
        return viewFactory.createCornerCell(gameView.getDimension)
      if (gameView.isCentralCell(c))
        return viewFactory.createCenterCell(gameView.getDimension)
      viewFactory.createNormalCell(gameView.getDimension)
    }

    override def restoreGame(): Unit = {
      cells.clear()
      gamePanel.removeAll()
    }

    override def getLabelPlayer: JLabel = playerOrWinnerLabel

    private def setColorBackground(c: Pair[Int], color: Color): Unit = {
      val button: JButton = cells(c)
      if (!(button.getBackground == colorProvider.getWhiteWinColor) && !(button.getBackground == colorProvider.getBlackWinColor)) {
        if (gameView.isCentralCell(c) || gameView.isCornerCell(c))
          button.setBackground(colorProvider.getSpecialCellColor)
        else
          button.setBackground(color)
      }
    }

  }
}
