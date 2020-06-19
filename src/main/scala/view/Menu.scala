package view

import java.awt.{Dimension, GridBagConstraints}
import java.awt.event.{ActionEvent, ActionListener}

import javax.swing._
import model.{GameMode, GameVariant, Level, Player}

trait Menu {

  /**
    * Gets player chosen.
    *
    * @return player
    */
  def getPlayer: Player.Val

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
  def getBoardVariant: GameVariant.Val

  /**
    * Gets the game mode chosen.
    *
    * @return game mode
    */
  def getGameMode: GameMode.Value

  /**
    * Gets the level of difficult chosen.
    *
    * @return level
    */
  def getDifficult: Level.Value

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
      new Dimension(ViewFactory.getSmallerSide, ViewFactory.getSmallerSide * 2 / 100)
    private var menuPanel, variantsPanel, diffPanel, playerChoicePanel, inGameMenuPanel,
                panelVariantHnefatafl, panelVariantTawlbwrdd, panelVariantTablut, panelVariantBrandubh,
                panelLevelNewcomer, panelLevelAmateur, panelLevelStandard, panelLevelAdvanced,
                panelChoseWhite, panelChoseBlack: JPanel = _
    private var pvpButton, pveButton, exitButtonMenu, hnefatafl, tawlbwrdd, tablut, brandubh,
      newcomer, amateur, standard, advanced, whiteButton, blackButton, quitGame, returnToMenu,
      returnToGame, restartGame: JButton = _

    private var labelHnefatafl, labelTawlbwrdd, labelTablut, labelBrandubh, labelNewcomer, labelAmateur,
                labelStandard, labelAdvanced, labelWhiteChose, labelBlackChose: JLabel = _

    private var gameMode: GameMode.Value = _
    private var boardVariant: GameVariant.Val = _
    private var levelIA: Level.Value = _
    private var player: Player.Val = _

    override def getPlayer: Player.Val = player

    override def initMenu: JPanel = {
      menuPanel = ViewFactory.createMenuPanel("Choose Mode: ")
      pveButton = ViewFactory.createMenuButton(" Player Vs Computer")
      pveButton.addActionListener(chooseGameModeListener("PVE"))
      pvpButton = ViewFactory.createMenuButton("Player Vs Player")
      pvpButton.addActionListener(chooseGameModeListener("PVP"))
      exitButtonMenu = ViewFactory.createMenuButton("Exit")
      exitButtonMenu.addActionListener(_ => System.exit(0))
      menuPanel.add(Box.createRigidArea(DIMENSION_PANEL))
      menuPanel.add(pveButton)
      menuPanel.add(pvpButton)
      menuPanel.add(exitButtonMenu)
      menuPanel.add(Box.createVerticalGlue)
      menuPanel
    }

    override def initVariantsMenu: JPanel = {
      variantsPanel = ViewFactory.createMenuPanel("Choose Board Variant: ")

      panelVariantHnefatafl = ViewFactory.createSubMenuPanel
      panelVariantTawlbwrdd = ViewFactory.createSubMenuPanel
      panelVariantTablut = ViewFactory.createSubMenuPanel
      panelVariantBrandubh = ViewFactory.createSubMenuPanel

      val limits: GridBagConstraints = new java.awt.GridBagConstraints()
      limits.gridy = 0
      limits.weightx = 1
      limits.fill = GridBagConstraints.NONE
      limits.anchor = GridBagConstraints.LINE_START

      labelHnefatafl = ViewFactory.createLabelBoardHnefatafl
      limits.gridx = 0
      panelVariantHnefatafl.add(labelHnefatafl, limits)
      hnefatafl = ViewFactory.createMenuButton("Hnefatafl (11 x 11)")
      hnefatafl.addActionListener(chooseVariantListener("Hnefatafl"))
      limits.gridx = 1
      panelVariantHnefatafl.add(hnefatafl, limits)

      labelTawlbwrdd = ViewFactory.createLabelBoardTawlbwrdd
      limits.gridx = 0
      panelVariantTawlbwrdd.add(labelTawlbwrdd, limits)
      tawlbwrdd = ViewFactory.createMenuButton("Tawlbwrdd (11 x 11)")
      tawlbwrdd.addActionListener(chooseVariantListener("Tawlbwrdd"))
      limits.gridx = 1
      panelVariantTawlbwrdd.add(tawlbwrdd, limits)

      labelTablut = ViewFactory.createLabelBoardTablut
      limits.gridx = 0
      panelVariantTablut.add(labelTablut, limits)
      tablut = ViewFactory.createMenuButton("Tablut (9 x 9)")
      tablut.addActionListener(chooseVariantListener("Tablut"))
      limits.gridx = 1
      panelVariantTablut.add(tablut, limits)

      labelBrandubh = ViewFactory.createLabelBoardBrandubh
      limits.gridx = 0
      panelVariantBrandubh.add(labelBrandubh, limits)
      brandubh = ViewFactory.createMenuButton("Brandubh (7 x 7)")
      brandubh.addActionListener(chooseVariantListener("Brandubh"))
      limits.gridx = 1
      panelVariantBrandubh.add(brandubh, limits)

      returnToMenu = ViewFactory.createMenuButton("Previous Menu")
      returnToMenu.addActionListener((_: ActionEvent) => view.switchOverlay(variantsPanel, menuPanel))

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

    override def initDiffMenu: JPanel = {
      diffPanel = ViewFactory.createMenuPanel("Choose Difficulty: ")

      panelLevelNewcomer = ViewFactory.createSubMenuPanel
      panelLevelAmateur = ViewFactory.createSubMenuPanel
      panelLevelStandard = ViewFactory.createSubMenuPanel
      panelLevelAdvanced = ViewFactory.createSubMenuPanel

      val limits: GridBagConstraints = new java.awt.GridBagConstraints()
      limits.gridy = 0
      limits.weightx = 1
      limits.fill = GridBagConstraints.NONE
      limits.anchor = GridBagConstraints.LINE_START

      labelNewcomer = ViewFactory.createLabelNewcomer
      limits.gridx = 0
      panelLevelNewcomer.add(labelNewcomer, limits)
      newcomer = ViewFactory.createMenuButton("Newcomer")
      newcomer.addActionListener(chooseLevelIAListener("Newcomer"))
      limits.gridx = 1
      panelLevelNewcomer.add(newcomer, limits)

      labelAmateur = ViewFactory.createLabelAmateur
      limits.gridx = 0
      panelLevelAmateur.add(labelAmateur, limits)
      amateur = ViewFactory.createMenuButton("Amateur")
      amateur.addActionListener(chooseLevelIAListener("Amateur"))
      limits.gridx = 1
      panelLevelAmateur.add(amateur, limits)

      labelStandard = ViewFactory.createLabelStandard
      limits.gridx = 0
      panelLevelStandard.add(labelStandard, limits)
      standard = ViewFactory.createMenuButton("Standard")
      standard.addActionListener(chooseLevelIAListener("Standard"))
      limits.gridx = 1
      panelLevelStandard.add(standard, limits)

      labelAdvanced = ViewFactory.createLabelAdvanced
      limits.gridx = 0
      panelLevelAdvanced.add(labelAdvanced, limits)
      advanced = ViewFactory.createMenuButton("Advanced")
      advanced.addActionListener(chooseLevelIAListener("Advanced"))
      limits.gridx = 1
      panelLevelAdvanced.add(advanced, limits)

      returnToMenu = ViewFactory.createMenuButton("Previous Menu")
      returnToMenu.addActionListener((_: ActionEvent) => view.switchOverlay(diffPanel, variantsPanel))

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

    override def initPlayerChoiceMenu: JPanel = {
      playerChoicePanel = ViewFactory.createMenuPanel("Choose Player: ")

      panelChoseWhite = ViewFactory.createSubMenuPanel
      panelChoseBlack = ViewFactory.createSubMenuPanel

      val limits: GridBagConstraints = new java.awt.GridBagConstraints()
      limits.gridy = 0
      limits.weightx = 0
      limits.fill = GridBagConstraints.NONE
      limits.anchor = GridBagConstraints.LINE_START

      labelWhiteChose = ViewFactory.createLabelWhitePlayer
      limits.gridx = 0
      panelChoseWhite.add(labelWhiteChose,limits)
      whiteButton = ViewFactory.createMenuButton("White Pawns")
      whiteButton.addActionListener(setPlayer("White"))
      limits.gridx = 1
      panelChoseWhite.add(whiteButton,limits)

      labelBlackChose = ViewFactory.createLabelBlackPlayer
      limits.gridx = 0
      panelChoseBlack.add(labelBlackChose,limits)
      blackButton = ViewFactory.createMenuButton("Black Pawns")
      blackButton.addActionListener(setPlayer("Black"))
      limits.gridx = 1
      panelChoseBlack.add(blackButton,limits)

      returnToMenu = ViewFactory.createMenuButton("Previous Menu")
      returnToMenu.addActionListener((_: ActionEvent) => view.switchOverlay(playerChoicePanel, variantsPanel))

      playerChoicePanel.add(Box.createRigidArea(DIMENSION_PANEL))
      playerChoicePanel.add(panelChoseWhite)
      playerChoicePanel.add(panelChoseBlack)
      playerChoicePanel.add(returnToMenu)
      playerChoicePanel.add(Box.createVerticalGlue)
      playerChoicePanel.setVisible(false)
      playerChoicePanel
    }

    override def getBoardVariant: GameVariant.Val = boardVariant

    override def getGameMode: GameMode.Value = gameMode

    override def getDifficult: Level.Value = levelIA

    override def initInGameMenu: JPanel = {
      inGameMenuPanel = ViewFactory.createMenuPanel("Choose Option: ")
      returnToGame = ViewFactory.createMenuButton("Return to Game")
      returnToGame.addActionListener((_: ActionEvent) => view.switchOverlay(inGameMenuPanel, view.getGamePanel))

      restartGame = ViewFactory.createMenuButton("Restart Match")
      restartGame.addActionListener((_: ActionEvent) => {view.initOrRestoreGUI(); view.switchOverlay(inGameMenuPanel, view.getGamePanel)})

      returnToMenu = ViewFactory.createMenuButton("Leave Match")
      returnToMenu.addActionListener((_: ActionEvent) => view.switchOverlay(inGameMenuPanel, menuPanel))
      quitGame = ViewFactory.createMenuButton("Quit Game")
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
      * Sets the game mode.
      *
      * @param gameModeString
      *               game mode chosen by the user.
      */
    private def setGameMode(gameModeString: String): Unit = gameModeString match {
      case "PVP" => gameMode = GameMode.PVP
      case "PVE" => gameMode = GameMode.PVE
    }

    /**
      * Listens for switch from main menù to variant menù.
      *
      * @param gameMode
      *             String of the game mode chosen.
      * @return listner
      */
    private def chooseGameModeListener(gameMode: String): ActionListener = (_: ActionEvent) => {
      setGameMode(gameMode)
      view.switchOverlay(menuPanel, variantsPanel)
    }

    /**
      * Sets the variant chosen from user.
      */
    private def setBoardVariant(variant: String): Unit = variant match {
      case "Hnefatafl" => boardVariant = GameVariant.Hnefatafl
      case "Tawlbwrdd" => boardVariant = GameVariant.Tawlbwrdd
      case "Tablut" => boardVariant = GameVariant.Tablut
      case "Brandubh" => boardVariant = GameVariant.Brandubh
    }

    /**
      * Listens for switch from variant menù in according to the chosen mode.
      *
      * @param variant
      *             String of the variant chosen.
      * @return listner
      */
    private def chooseVariantListener(variant: String): ActionListener = (_: ActionEvent) => {
      setBoardVariant(variant)
      gameMode match {
        case GameMode.PVP => view.switchOverlay(variantsPanel, playerChoicePanel)
        case GameMode.PVE => view.switchOverlay(variantsPanel, diffPanel)
      }
    }

    /**
      * Sets the difficult chosen from user.
      */
    private def setLevelIA(level: String): Unit = level match {
      case "Newcomer" => levelIA = Level.Newcomer
      case "Amateur" => levelIA = Level.Amateur
      case "Standard" => levelIA = Level.Standard
      case "Advanced" => levelIA = Level.Advanced
    }

    /**
      * Listens for switch from difficult menù.
      *
      * @param level
      *             String of the level IA chosen.
      * @return listner
      */
    private def chooseLevelIAListener(level: String): ActionListener = (_: ActionEvent) => {
      setLevelIA(level)
      view.switchOverlay(diffPanel, playerChoicePanel)
    }

    /**
      * Sets the user player.
      */
    private def setPlayer(playerString: String): ActionListener = (_: ActionEvent) => playerString match {
      case "White" => player = Player.White; initOrRestore()
      case "Black" => player = Player.Black; initOrRestore()
    }

    /**
      * Initializes or restores the game by calling game view.
      */
    private def initOrRestore(): Unit = {
      view.initOrRestoreGUI()
    }
  }
}
