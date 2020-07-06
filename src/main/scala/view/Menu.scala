package view

import java.awt.{Dimension, GridBagConstraints}
import java.awt.event.{ActionEvent, ActionListener}

import javax.swing._
import model.GameMode.GameMode
import model.GameVariant.GameVariant
import model.Level.Level
import model.Player.Player
import model.{GameMode, GameVariant, Level, Player}

trait Menu {

  /**
    * Gets player chosen.
    *
    * @return player
    */
  def getPlayer: Player

  /**
    * Initializes the main menù.
    *
    * @return panel
    */
  def initMenu: JPanel

  /**
    * Initializes the variant menù.
    *
    * @return panel
    */
  def initVariantsMenu: JPanel

  /**
    * Initializes the difficult selection menù.
    *
    * @return panel
    */
  def initDiffMenu: JPanel

  /**
    * Initializes the player selection menù.
    *
    * @return panel
    */
  def initPlayerChoiceMenu: JPanel

  /**
    * Gets the variant chosen.
    *
    * @return variant
    */
  def getBoardVariant: GameVariant

  /**
    * Gets the game mode chosen.
    *
    * @return game mode
    */
  def getGameMode: GameMode

  /**
    * Gets the level of difficult chosen.
    *
    * @return level
    */
  def getDifficulty: Level

  /**
    * Initializes the game menù.
    *
    * @return panel
    */
  def initInGameMenu: JPanel
}

object Menu {

  def apply(gameView: ViewHnefatafl): Menu = MenuImpl(gameView)

  case class MenuImpl(view: ViewHnefatafl) extends Menu {

    private final val DIMENSION_PANEL: Dimension =
      new Dimension(GameFactory.getSmallerSide, GameFactory.getSmallerSide * 2 / 100)
    private var menuPanel, variantsPanel, diffPanel, playerChoicePanel, inGameMenuPanel,
                panelVariantHnefatafl, panelVariantTawlbwrdd, panelVariantTablut, panelVariantBrandubh,
                panelLevelNewcomer, panelLevelAmateur, panelLevelStandard, panelLevelAdvanced,
                panelChoseWhite, panelChoseBlack: JPanel = _
    private var pvpButton, pveButton, exitButtonMenu, quitGame, returnToMenu,
      returnToGame, restartGame: JButton = _

    private var gameMode: GameMode = _
    private var boardVariant: GameVariant = _
    private var levelIA: Level = _
    private var player: Player = _

    private var limits: GridBagConstraints = _

    override def getPlayer: Player = player

    override def initMenu: JPanel = {
      menuPanel = MenuFactory.createMenuPanel("Choose Mode: ")
      pveButton = MenuFactory.createMainButton(GameMode.PVE.toString)
      pveButton.addActionListener(chooseGameModeListener(GameMode.PVE.toString))
      pvpButton = MenuFactory.createMainButton(GameMode.PVP.toString)
      pvpButton.addActionListener(chooseGameModeListener(GameMode.PVP.toString))
      exitButtonMenu = MenuFactory.createMainButton("Exit")
      exitButtonMenu.addActionListener(_ => System.exit(0))
      menuPanel.add(Box.createRigidArea(DIMENSION_PANEL))
      menuPanel.add(pveButton)
      menuPanel.add(pvpButton)
      menuPanel.add(exitButtonMenu)
      menuPanel.add(Box.createVerticalGlue)
      limits = GameFactory.createBagConstraints
      limits.fill = GridBagConstraints.NONE
      limits.anchor = GridBagConstraints.LINE_START
      menuPanel
    }

    override def initVariantsMenu: JPanel = {
      variantsPanel = MenuFactory.createMenuPanel("Choose Game Variant: ")

      panelVariantHnefatafl = MenuFactory.createSubMenuVariantPanel
      panelVariantTawlbwrdd = MenuFactory.createSubMenuVariantPanel
      panelVariantTablut = MenuFactory.createSubMenuVariantPanel
      panelVariantBrandubh = MenuFactory.createSubMenuVariantPanel

      initVariantButton(panelVariantHnefatafl, MenuFactory.createVariantButton(GameVariant.Hnefatafl.toString),
        MenuFactory.createLabelBoardHnefatafl, GameVariant.Hnefatafl.toString)
      initVariantButton(panelVariantTawlbwrdd, MenuFactory.createVariantButton(GameVariant.Tawlbwrdd.toString),
        MenuFactory.createLabelBoardTawlbwrdd, GameVariant.Tawlbwrdd.toString)
      initVariantButton(panelVariantTablut, MenuFactory.createVariantButton(GameVariant.Tablut.toString),
        MenuFactory.createLabelBoardTablut, GameVariant.Tablut.toString)
      initVariantButton(panelVariantBrandubh, MenuFactory.createVariantButton(GameVariant.Brandubh.toString),
        MenuFactory.createLabelBoardBrandubh, GameVariant.Brandubh.toString)

      initReturnButton(variantsPanel, menuPanel, "Previous Menu")

      variantsPanel.add(Box.createRigidArea(DIMENSION_PANEL))
      variantsPanel.add(panelVariantHnefatafl)
      variantsPanel.add(panelVariantTawlbwrdd)
      variantsPanel.add(panelVariantTablut)
      variantsPanel.add(panelVariantBrandubh)
      variantsPanel.add(returnToMenu)
      variantsPanel.add(Box.createVerticalGlue)

      variantsPanel.setVisible(false)
      variantsPanel
    }

    private def initVariantButton(myPanel: JPanel, variantButton: JButton, label: JLabel, text: String): Unit = {
      GameFactory.resetXConstraints(limits)
      myPanel.add(label, limits)
      variantButton.addActionListener(chooseVariantListener(text))
      GameFactory.incrementXConstraints(limits)
      myPanel.add(variantButton, limits)
    }

    override def initDiffMenu: JPanel = {
      diffPanel = MenuFactory.createMenuPanel("Choose Difficulty: ")

      panelLevelNewcomer = MenuFactory.createSubMenuLevelPanel
      panelLevelAmateur = MenuFactory.createSubMenuLevelPanel
      panelLevelStandard = MenuFactory.createSubMenuLevelPanel
      panelLevelAdvanced = MenuFactory.createSubMenuLevelPanel

      initDiffButton(panelLevelNewcomer, MenuFactory.createLevelButton(Level.Newcomer.toString), MenuFactory.createLabelNewcomer, Level.Newcomer.toString)
      initDiffButton(panelLevelAmateur, MenuFactory.createLevelButton(Level.Amateur.toString), MenuFactory.createLabelAmateur, Level.Amateur.toString)
      initDiffButton(panelLevelStandard, MenuFactory.createLevelButton(Level.Standard.toString), MenuFactory.createLabelStandard, Level.Standard.toString)
      initDiffButton(panelLevelAdvanced, MenuFactory.createLevelButton(Level.Advanced.toString), MenuFactory.createLabelAdvanced, Level.Advanced.toString)

      initReturnButton(diffPanel, variantsPanel, "Previous Menu")

      diffPanel.add(Box.createRigidArea(DIMENSION_PANEL))
      diffPanel.add(panelLevelNewcomer)
      diffPanel.add(panelLevelAmateur)
      diffPanel.add(panelLevelStandard)
      diffPanel.add(panelLevelAdvanced)
      diffPanel.add(returnToMenu)
      diffPanel.add(Box.createVerticalGlue)
      diffPanel.setVisible(false)
      diffPanel
    }

    private def initDiffButton(myPanel: JPanel, diffButton: JButton, label: JLabel, text: String): Unit = {
      GameFactory.resetXConstraints(limits)
      myPanel.add(label, limits)
      diffButton.addActionListener(chooseLevelIAListener(text))
      GameFactory.incrementXConstraints(limits)
      myPanel.add(diffButton, limits)
    }

    override def initPlayerChoiceMenu: JPanel = {
      playerChoicePanel = MenuFactory.createMenuPanel("Choose Player: ")

      panelChoseWhite = MenuFactory.createSubMenuPlayerPanel
      panelChoseBlack = MenuFactory.createSubMenuPlayerPanel

      initPlayerChoiceButton(panelChoseWhite, MenuFactory.createPlayerButton(Player.White.toString),
        MenuFactory.createLabelWhitePlayer, Player.White.toString)
      initPlayerChoiceButton(panelChoseBlack, MenuFactory.createPlayerButton(Player.Black.toString),
        MenuFactory.createLabelBlackPlayer, Player.Black.toString)

      initReturnButton(playerChoicePanel, variantsPanel, "Previous Menu")

      playerChoicePanel.add(Box.createRigidArea(DIMENSION_PANEL))
      playerChoicePanel.add(panelChoseWhite)
      playerChoicePanel.add(panelChoseBlack)
      playerChoicePanel.add(returnToMenu)
      playerChoicePanel.add(Box.createVerticalGlue)
      playerChoicePanel.setVisible(false)
      playerChoicePanel
    }

    private def initPlayerChoiceButton(myPanel: JPanel, diffButton: JButton, label: JLabel, text: String): Unit = {
      GameFactory.resetXConstraints(limits)
      myPanel.add(label, limits)
      diffButton.addActionListener(selectPlayer(text))
      GameFactory.incrementXConstraints(limits)
      myPanel.add(diffButton, limits)
    }

    private def initReturnButton(previousPanel: JPanel, nextPanel: JPanel, text: String): Unit = {
      returnToMenu = MenuFactory.createMainButton(text)
      returnToMenu.addActionListener((_: ActionEvent) => view.switchOverlay(previousPanel, nextPanel))
    }

    override def getBoardVariant: GameVariant = boardVariant

    override def getGameMode: GameMode = gameMode

    override def getDifficulty: Level = levelIA

    override def initInGameMenu: JPanel = {
      inGameMenuPanel = MenuFactory.createMenuPanel("Choose Option: ")
      returnToGame = MenuFactory.createMainButton("Return to Game")
      returnToGame.addActionListener((_: ActionEvent) => view.switchOverlay(inGameMenuPanel, view.getGamePanel))

      restartGame = MenuFactory.createMainButton("Restart Match")
      restartGame.addActionListener((_: ActionEvent) => {view.resetGUI(); view.switchOverlay(inGameMenuPanel, view.getGamePanel)})

      initReturnButton(inGameMenuPanel, menuPanel, "Leave Match")

      quitGame = MenuFactory.createMainButton("Quit Game")
      quitGame.addActionListener((_: ActionEvent) => System.exit(0))
      inGameMenuPanel.add(Box.createRigidArea(DIMENSION_PANEL))
      inGameMenuPanel.add(returnToGame)
      inGameMenuPanel.add(restartGame)
      inGameMenuPanel.add(returnToMenu)
      inGameMenuPanel.add(quitGame)
      inGameMenuPanel.add(Box.createVerticalGlue)
      inGameMenuPanel.setVisible(false)
      inGameMenuPanel
    }

    /**
      * Listens for switch from main menù to variant menù.
      *
      * @param gameModeString
      *             String of the game mode chosen.
      * @return listner
      */
    private def chooseGameModeListener(gameModeString: String): ActionListener =
      (_: ActionEvent) => {
        gameMode = GameMode.withName(gameModeString)
        view.switchOverlay(menuPanel, variantsPanel)
      }

    /**
      * Listens for switch from variant menù in according to the chosen mode.
      *
      * @param variant
      *             String of the variant chosen.
      * @return listner
      */
    private def chooseVariantListener(variant: String): ActionListener = (_: ActionEvent) => {
      boardVariant = GameVariant.withName(variant)
      gameMode match {
        case GameMode.PVP => view.switchOverlay(variantsPanel, playerChoicePanel)
        case GameMode.PVE => view.switchOverlay(variantsPanel, diffPanel)
      }
    }

    /**
      * Listens for switch from difficult menù.
      *
      * @param level
      *             String of the level IA chosen.
      * @return listner
      */
    private def chooseLevelIAListener(level: String): ActionListener =
      (_: ActionEvent) => {
        levelIA = Level.withName(level)
        view.switchOverlay(diffPanel, playerChoicePanel)
      }

    private def selectPlayer(playerString: String): ActionListener =
      (_: ActionEvent) => {
        player = Player.withName(playerString)
        view.resetGUI()
      }

  }
}
