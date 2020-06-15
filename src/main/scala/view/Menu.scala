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

  def apply(viewFactory: ViewFactory, gameView: GameView): Menu = MenuImpl(viewFactory, gameView)

  case class MenuImpl(viewFactory: ViewFactory, gameView: GameView) extends Menu {
    private var menuPanel, variantsPanel, diffPanel, playerChoicePanel, inGameMenuPanel: JPanel = _
    private var pvpButton, pveButton, exitButtonMenu, hnefatafl, tawlbwrdd, tablut, brandubh,
      newcomer, amateur, standard, advanced, whiteButton, blackButton, quitGame, returnToMenu,
      returnToGame: JButton = _
    private val smallerSide = viewFactory.getSmallerSide
    private var gameMode: GameMode.Value = _
    private var boardVariant: GameVariant.Val = _
    private var player: Player.Value = _

    def initMenu: JPanel = {
      menuPanel = viewFactory.createMenuPanel("Choose Mode: ")
      pveButton = viewFactory.createMenuButton(" Player Vs Computer")
      pveButton.addActionListener(_ => pveVariant())
      pvpButton = viewFactory.createMenuButton("Player Vs Player")
      pvpButton.addActionListener(_ => pvpVariant())
      exitButtonMenu = viewFactory.createMenuButton("Exit")
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
      variantsPanel = viewFactory.createMenuPanel("Choose Board Variant: ")
      hnefatafl = viewFactory.createMenuButton("Hnefatafl(11 x 11)")
      hnefatafl.addActionListener(chooseListener("Hnefatafl"))
      tawlbwrdd = viewFactory.createMenuButton("Tawlbwrdd(11 x 11)")
      tawlbwrdd.addActionListener(chooseListener("Tawlbwrdd"))
      tablut = viewFactory.createMenuButton("Tablut(9 x 9)")
      tablut.addActionListener(chooseListener("Tablut"))
      brandubh = viewFactory.createMenuButton("Brandubh(7 x 7)")
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
      diffPanel = viewFactory.createMenuPanel("Choose Difficulty: ")
      newcomer = viewFactory.createMenuButton("Newcomer")
      newcomer.addActionListener((_: ActionEvent) => gameView.showOverlay(diffPanel, playerChoicePanel))
      amateur = viewFactory.createMenuButton("Amateur")
      amateur.addActionListener((_: ActionEvent) => gameView.showOverlay(diffPanel, playerChoicePanel))
      standard = viewFactory.createMenuButton("Standard")
      standard.addActionListener((_: ActionEvent) => gameView.showOverlay(diffPanel, playerChoicePanel))
      advanced = viewFactory.createMenuButton("Advanced")
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
      playerChoicePanel = viewFactory.createMenuPanel("Choose Player: ")
      whiteButton = viewFactory.createMenuButton("White Pawns")
      whiteButton.addActionListener((_: ActionEvent) => whitePlayer())
      blackButton = viewFactory.createMenuButton("Black Pawns")
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
      inGameMenuPanel = viewFactory.createMenuPanel("Choose Option: ")
      returnToGame = viewFactory.createMenuButton("Return to Game")
      returnToGame.addActionListener((_: ActionEvent) => gameView.showOverlay(inGameMenuPanel, gameView.getGamePanel))
      returnToMenu = viewFactory.createMenuButton("Leave Match")
      returnToMenu.addActionListener((_: ActionEvent) => gameView.showOverlay(inGameMenuPanel, menuPanel))
      quitGame = viewFactory.createMenuButton("Quit Game")
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
