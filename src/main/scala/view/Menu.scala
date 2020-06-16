package view

import java.awt.Dimension
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.{Box, JButton, JPanel}
import model.{GameMode, GameVariant, Player, Level}

trait Menu {

  /**
    * Gets player chosen.
    *
    * @return player
    */
  def getPlayer: Player.Value

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
    * Initializes the game menù.
    *
    * @return panel
    */
  def initInGameMenu: JPanel
}

object Menu {

  def apply(viewFactory: ViewFactory, gameView: GameView): Menu = MenuImpl(viewFactory, gameView)

  case class MenuImpl(viewFactory: ViewFactory, gameView: GameView) extends Menu {

    private val smallerSide = viewFactory.getSmallerSide
    private final val DIMENSION_PANEL: Dimension = new Dimension(smallerSide, smallerSide * 2 / 100)
    private var menuPanel, variantsPanel, diffPanel, playerChoicePanel, inGameMenuPanel: JPanel = _
    private var pvpButton, pveButton, exitButtonMenu, hnefatafl, tawlbwrdd, tablut, brandubh,
      newcomer, amateur, standard, advanced, whiteButton, blackButton, quitGame, returnToMenu,
      returnToGame, restartGame: JButton = _

    private var gameMode: GameMode.Value = _
    private var boardVariant: GameVariant.Val = _
    private var levelIA: Level.Value = _
    private var player: Player.Value = _

    override def getPlayer: Player.Value = player

    override def initMenu: JPanel = {
      menuPanel = viewFactory.createMenuPanel("Choose Mode: ")
      pveButton = viewFactory.createMenuButton(" Player Vs Computer")
      pveButton.addActionListener(chooseGameModeListener("PVE"))
      pvpButton = viewFactory.createMenuButton("Player Vs Player")
      pvpButton.addActionListener(chooseGameModeListener("PVP"))
      exitButtonMenu = viewFactory.createMenuButton("Exit")
      exitButtonMenu.addActionListener(_ => System.exit(0))
      menuPanel.add(Box.createRigidArea(DIMENSION_PANEL))
      menuPanel.add(pveButton)
      menuPanel.add(pvpButton)
      menuPanel.add(exitButtonMenu)
      menuPanel.add(Box.createVerticalGlue)
      menuPanel
    }

    override def initVariantsMenu: JPanel = {
      variantsPanel = viewFactory.createMenuPanel("Choose Board Variant: ")
      hnefatafl = viewFactory.createMenuButton("Hnefatafl (11 x 11)")
      hnefatafl.addActionListener(chooseVariantListener("Hnefatafl"))
      tawlbwrdd = viewFactory.createMenuButton("Tawlbwrdd (11 x 11)")
      tawlbwrdd.addActionListener(chooseVariantListener("Tawlbwrdd"))
      tablut = viewFactory.createMenuButton("Tablut (9 x 9)")
      tablut.addActionListener(chooseVariantListener("Tablut"))
      brandubh = viewFactory.createMenuButton("Brandubh (7 x 7)")
      brandubh.addActionListener(chooseVariantListener("Brandubh"))

      returnToMenu = viewFactory.createMenuButton("Previous Menu")
      returnToMenu.addActionListener((_: ActionEvent) => gameView.switchOverlay(variantsPanel, menuPanel))

      variantsPanel.add(Box.createRigidArea(DIMENSION_PANEL))
      variantsPanel.add(hnefatafl)
      variantsPanel.add(tawlbwrdd)
      variantsPanel.add(tablut)
      variantsPanel.add(brandubh)
      variantsPanel.add(returnToMenu)
      variantsPanel.add(Box.createVerticalGlue)
      variantsPanel.setVisible(false)
      variantsPanel
    }

    override def initDiffMenu: JPanel = {
      diffPanel = viewFactory.createMenuPanel("Choose Difficulty: ")
      newcomer = viewFactory.createMenuButton("Newcomer")
      newcomer.addActionListener(chooseLevelIAListener("Newcomer"))
      amateur = viewFactory.createMenuButton("Amateur")
      amateur.addActionListener(chooseLevelIAListener("Amateur"))
      standard = viewFactory.createMenuButton("Standard")
      standard.addActionListener(chooseLevelIAListener("Standard"))
      advanced = viewFactory.createMenuButton("Advanced")
      advanced.addActionListener(chooseLevelIAListener("Advanced"))

      returnToMenu = viewFactory.createMenuButton("Previous Menu")
      returnToMenu.addActionListener((_: ActionEvent) => gameView.switchOverlay(diffPanel, variantsPanel))

      diffPanel.add(Box.createRigidArea(DIMENSION_PANEL))
      diffPanel.add(newcomer)
      diffPanel.add(amateur)
      diffPanel.add(standard)
      diffPanel.add(advanced)
      diffPanel.add(returnToMenu)
      diffPanel.add(Box.createVerticalGlue)
      diffPanel.setVisible(false)
      diffPanel
    }

    override def initPlayerChoiceMenu: JPanel = {
      playerChoicePanel = viewFactory.createMenuPanel("Choose Player: ")
      whiteButton = viewFactory.createMenuButton("White Pawns")
      whiteButton.addActionListener(setPlayer("White"))
      blackButton = viewFactory.createMenuButton("Black Pawns")
      blackButton.addActionListener(setPlayer("Black"))

      returnToMenu = viewFactory.createMenuButton("Previous Menu")
      returnToMenu.addActionListener((_: ActionEvent) => gameView.switchOverlay(playerChoicePanel, variantsPanel))

      playerChoicePanel.add(Box.createRigidArea(DIMENSION_PANEL))
      playerChoicePanel.add(whiteButton)
      playerChoicePanel.add(blackButton)
      playerChoicePanel.add(returnToMenu)
      playerChoicePanel.add(Box.createVerticalGlue)
      playerChoicePanel.setVisible(false)
      playerChoicePanel
    }

    override def getBoardVariant: GameVariant.Val = boardVariant

    override def initInGameMenu: JPanel = {
      inGameMenuPanel = viewFactory.createMenuPanel("Choose Option: ")
      returnToGame = viewFactory.createMenuButton("Return to Game")
      returnToGame.addActionListener((_: ActionEvent) => gameView.switchOverlay(inGameMenuPanel, gameView.getGamePanel))

      restartGame = viewFactory.createMenuButton("Restart Match")
      restartGame.addActionListener((_: ActionEvent) => {gameView.initOrRestoreGUI(); gameView.switchOverlay(inGameMenuPanel, gameView.getGamePanel)})

      returnToMenu = viewFactory.createMenuButton("Leave Match")
      returnToMenu.addActionListener((_: ActionEvent) => gameView.switchOverlay(inGameMenuPanel, menuPanel))
      quitGame = viewFactory.createMenuButton("Quit Game")
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
      gameView.switchOverlay(menuPanel, variantsPanel)
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
        case GameMode.PVP => gameView.switchOverlay(variantsPanel, playerChoicePanel)
        case GameMode.PVE => gameView.switchOverlay(variantsPanel, diffPanel)
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
      gameView.switchOverlay(diffPanel, playerChoicePanel)
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
      gameView.initOrRestoreGUI()
    }
  }
}
