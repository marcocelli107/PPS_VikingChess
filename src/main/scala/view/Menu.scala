package view

import java.awt.Dimension
import java.awt.event.{ActionEvent, ActionListener}

import javax.swing.{Box, JButton, JPanel}
import model.{GameMode, GameVariant, Player}

trait Menu {
  def getPlayer: Player.Value
  def initMenu: JPanel
  def initVariantsMenu: JPanel
  def initDiffMenu: JPanel
  def initPlayerChoiceMenu: JPanel
  def getBoardVariant: GameVariant.Val
  def initInGameMenu: JPanel
}

object Menu {

  def apply(gameView: GameView): Menu = MenuImpl(gameView)

  case class MenuImpl(gameView: GameView) extends Menu {
    private var menuPanel, variantsPanel, diffPanel, playerChoicePanel, inGameMenuPanel: JPanel = _
    private var pvpButton, pveButton, exitButtonMenu, hnefatafl, tawlbwrdd, tablut, brandubh,
      newcomer, amateur, standard, advanced, whiteButton, blackButton, quitGame, returnToMenu,
      returnToGame: JButton = _
    private val smallerSide = ViewFactory.getSmallerSide
    private var gameMode: GameMode.Value = _
    private var boardVariant: GameVariant.Val = _
    private var player: Player.Value = _

    def initMenu: JPanel = {
      menuPanel = ViewFactory.createMenuPanel("Choose Mode: ")
      pveButton = ViewFactory.createMenuButton(" Player Vs Computer")
      pveButton.addActionListener(_ => pveVariant())
      pvpButton = ViewFactory.createMenuButton("Player Vs Player")
      pvpButton.addActionListener(_ => pvpVariant())
      exitButtonMenu = ViewFactory.createMenuButton("Exit")
      exitButtonMenu.addActionListener(_ => System.exit(0))
      menuPanel.add(Box.createRigidArea(new Dimension(smallerSide, smallerSide * 2 / 100)))
      menuPanel.add(pveButton)
      menuPanel.add(pvpButton)
      menuPanel.add(exitButtonMenu)
      menuPanel.add(Box.createVerticalGlue)
      menuPanel
    }

    def pveVariant(): Unit = {
      gameMode = GameMode.PVE
      showVariant()
    }

    def pvpVariant(): Unit = {
      gameMode = GameMode.PVP
      showVariant()
    }

    def showVariant(): Unit = {
      gameView.showOverlay(menuPanel, variantsPanel)
    }

    def setBoardVariant(variant: String): Unit = {
      if (variant eq GameVariant.Hnefatafl.nameVariant) boardVariant = GameVariant.Hnefatafl
      else if (variant eq GameVariant.Tawlbwrdd.nameVariant) boardVariant = GameVariant.Tawlbwrdd
      else if (variant eq GameVariant.Tablut.nameVariant) boardVariant = GameVariant.Tablut
      else if (variant eq GameVariant.Brandubh.nameVariant) boardVariant = GameVariant.Brandubh
    }

    def chooseListener(text: String): ActionListener = (_: ActionEvent) => {
      setBoardVariant(text)
      if (gameMode eq GameMode.PVP) gameView.showOverlay(variantsPanel, playerChoicePanel)
      else gameView.showOverlay(variantsPanel, diffPanel)
    }

    def initVariantsMenu: JPanel = {
      variantsPanel = ViewFactory.createMenuPanel("Choose Board Variant: ")
      hnefatafl = ViewFactory.createMenuButton("Hnefatafl(11 x 11)")
      hnefatafl.addActionListener(chooseListener("Hnefatafl"))
      tawlbwrdd = ViewFactory.createMenuButton("Tawlbwrdd(11 x 11)")
      tawlbwrdd.addActionListener(chooseListener("Tawlbwrdd"))
      tablut = ViewFactory.createMenuButton("Tablut(9 x 9)")
      tablut.addActionListener(chooseListener("Tablut"))
      brandubh = ViewFactory.createMenuButton("Brandubh(7 x 7)")
      brandubh.addActionListener(chooseListener("Brandubh"))
      variantsPanel.add(Box.createRigidArea(new Dimension(smallerSide, smallerSide * 2 / 100)))
      variantsPanel.add(hnefatafl)
      variantsPanel.add(tawlbwrdd)
      variantsPanel.add(tablut)
      variantsPanel.add(brandubh)
      variantsPanel.add(Box.createVerticalGlue)
      variantsPanel.setVisible(false)
      variantsPanel
    }

    def initDiffMenu: JPanel = {
      diffPanel = ViewFactory.createMenuPanel("Choose Difficulty: ")
      newcomer = ViewFactory.createMenuButton("Newcomer")
      newcomer.addActionListener((_: ActionEvent) => gameView.showOverlay(diffPanel, playerChoicePanel))
      amateur = ViewFactory.createMenuButton("Amateur")
      amateur.addActionListener((_: ActionEvent) => gameView.showOverlay(diffPanel, playerChoicePanel))
      standard = ViewFactory.createMenuButton("Standard")
      standard.addActionListener((_: ActionEvent) => gameView.showOverlay(diffPanel, playerChoicePanel))
      advanced = ViewFactory.createMenuButton("Advanced")
      advanced.addActionListener((_: ActionEvent) => gameView.showOverlay(diffPanel, playerChoicePanel))
      diffPanel.add(Box.createRigidArea(new Dimension(smallerSide, smallerSide * 2 / 100)))
      diffPanel.add(newcomer)
      diffPanel.add(amateur)
      diffPanel.add(standard)
      diffPanel.add(advanced)
      diffPanel.add(Box.createVerticalGlue)
      diffPanel.setVisible(false)
      diffPanel
    }

    def initPlayerChoiceMenu: JPanel = {
      playerChoicePanel = ViewFactory.createMenuPanel("Choose Player: ")
      whiteButton = ViewFactory.createMenuButton("White Pawns")
      whiteButton.addActionListener((_: ActionEvent) => whitePlayer())
      blackButton = ViewFactory.createMenuButton("Black Pawns")
      blackButton.addActionListener((_: ActionEvent) => blackPlayer())
      playerChoicePanel.add(Box.createRigidArea(new Dimension(smallerSide, smallerSide * 2 / 100)))
      playerChoicePanel.add(whiteButton)
      playerChoicePanel.add(blackButton)
      playerChoicePanel.add(Box.createVerticalGlue)
      playerChoicePanel.setVisible(false)
      playerChoicePanel
    }

    private def initOrRestore(): Unit = {
      gameView.initOrRestoreGUI
    }

    def whitePlayer(): Unit = {
      player = Player.White
      initOrRestore()
    }

    def blackPlayer(): Unit = {
      player = Player.Black
      initOrRestore()
    }

    def getPlayer: Player.Value = player

    def getBoardVariant: GameVariant.Val = boardVariant

    def initInGameMenu: JPanel = {
      inGameMenuPanel = ViewFactory.createMenuPanel("Choose Option: ")
      returnToGame = ViewFactory.createMenuButton("Return to Game")
      returnToGame.addActionListener((_: ActionEvent) => gameView.showOverlay(inGameMenuPanel, gameView.getGamePanel))
      returnToMenu = ViewFactory.createMenuButton("Leave Match")
      returnToMenu.addActionListener((_: ActionEvent) => gameView.showOverlay(inGameMenuPanel, menuPanel))
      quitGame = ViewFactory.createMenuButton("Quit Game")
      quitGame.addActionListener((_: ActionEvent) => System.exit(0))
      inGameMenuPanel.add(Box.createRigidArea(new Dimension(smallerSide, smallerSide * 2 / 100)))
      inGameMenuPanel.add(returnToGame)
      inGameMenuPanel.add(returnToMenu)
      inGameMenuPanel.add(quitGame)
      inGameMenuPanel.add(Box.createVerticalGlue)
      inGameMenuPanel.setVisible(false)
      inGameMenuPanel
    }
  }
}
