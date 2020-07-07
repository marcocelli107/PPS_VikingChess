package view.menu

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{Dimension, GridBagConstraints}

import javax.swing._
import model.game.GameMode.GameMode
import model.game.GameVariant.GameVariant
import model.game.Level.Level
import model.game.Player.Player
import model.game.{GameMode, GameVariant, Level, Player}
import view.ViewHnefatafl
import view.factories.{GameFactory, MenuFactory}
import view.utils.GridConstraints
import view.utils.JPanelAddAll._

trait ViewMenu {

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

object ViewMenu {

  def apply(gameView: ViewHnefatafl): ViewMenu = ViewMenuImpl(gameView)

  case class ViewMenuImpl(view: ViewHnefatafl) extends ViewMenu {

    private val DIMENSION_PANEL: Dimension =
      new Dimension(GameFactory.getSmallerSide, GameFactory.getSmallerSide * 2 / 100)
    private var menuPanel, variantsPanel, diffPanel, playerChoicePanel, inGameMenuPanel,
                panelVariantHnefatafl, panelVariantTawlbwrdd, panelVariantTablut, panelVariantBrandubh,
                panelLevelNewcomer, panelLevelStandard, panelLevelAdvanced,
                panelChoseWhite, panelChoseBlack: JPanel = _
    private var pvpButton, pveButton, exitButtonMenu, quitGame, returnToMenu,
      returnToGame, restartGame: JButton = _

    private var gameMode: GameMode = _
    private var boardVariant: GameVariant = _
    private var levelIA: Level = _
    private var chosenPlayer: Player = _

    val chooseModeString: String = "Choose Mode: "
    val exitString: String = "Exit"
    val chooseVariantString: String = "Choose Game Variant: "
    val previousMenuString: String = "Previous Menu"
    val chooseLevelString: String = "Choose Difficulty: "
    val choosePlayerString: String = "Choose Player: "
    val chooseOptionString: String = "Choose Option: "
    val returnToGameString: String = "Return to Game"
    val restartMatchString: String = "Restart Match"
    val leaveMatchString: String = "Leave Match"
    val quitGameString: String = "Quit Game"
    val pawnButtonString: String = " pawns"

    override def getPlayer: Player = chosenPlayer

    override def initMenu: JPanel = {
      menuPanel = MenuFactory.createMenuPanel(chooseModeString)
      pveButton = MenuFactory.createMainButton(GameMode.PVE.toString)
      pveButton.addActionListener(selectGameModeListener(GameMode.PVE.toString))
      pvpButton = MenuFactory.createMainButton(GameMode.PVP.toString)
      pvpButton.addActionListener(selectGameModeListener(GameMode.PVP.toString))
      exitButtonMenu = MenuFactory.createMainButton(exitString)
      exitButtonMenu.addActionListener(_ => System.exit(0))
      menuPanel.addAll(Box.createRigidArea(DIMENSION_PANEL))(pveButton)(pvpButton)(exitButtonMenu)(Box.createVerticalGlue)
      menuPanel
    }

    private def initMenuButton(p: JPanel, l: JLabel, b: JButton, al: ActionListener): Unit = {
      GridConstraints.setAnchor(GridBagConstraints.LINE_START)
      GridConstraints.resetXConstraints()
      p.add(l, GridConstraints.getLimits)
      b.addActionListener(al)
      GridConstraints.incrementXConstraints()
      p.add(b, GridConstraints.getLimits)
    }

    override def initVariantsMenu: JPanel = {
      variantsPanel = MenuFactory.createMenuPanel(chooseVariantString)

      panelVariantHnefatafl = MenuFactory.createSubMenuVariantPanel
      panelVariantTawlbwrdd = MenuFactory.createSubMenuVariantPanel
      panelVariantTablut = MenuFactory.createSubMenuVariantPanel
      panelVariantBrandubh = MenuFactory.createSubMenuVariantPanel

      initMenuButton(panelVariantHnefatafl, MenuFactory.createLabelBoardHnefatafl,
        MenuFactory.createVariantButton(variantButtonString(GameVariant.Hnefatafl)),
        selectVariantListener(GameVariant.Hnefatafl))
      initMenuButton(panelVariantTawlbwrdd, MenuFactory.createLabelBoardTawlbwrdd,
        MenuFactory.createVariantButton(variantButtonString(GameVariant.Tawlbwrdd)),
        selectVariantListener(GameVariant.Tawlbwrdd))
      initMenuButton(panelVariantTablut, MenuFactory.createLabelBoardTablut,
        MenuFactory.createVariantButton(variantButtonString(GameVariant.Tablut)),
        selectVariantListener(GameVariant.Tablut))
      initMenuButton(panelVariantBrandubh, MenuFactory.createLabelBoardBrandubh,
        MenuFactory.createVariantButton(variantButtonString(GameVariant.Brandubh)),
        selectVariantListener(GameVariant.Brandubh))

      initPreviousMenuButton(variantsPanel, menuPanel)

      variantsPanel.addAll(Box.createRigidArea(DIMENSION_PANEL))(panelVariantHnefatafl)(panelVariantTawlbwrdd)(
        panelVariantTablut)(panelVariantBrandubh)(returnToMenu)(Box.createVerticalGlue)

      variantsPanel.setVisible(false)
      variantsPanel
    }

    private def variantButtonString(variant: GameVariant): String =
      variant.toString + " (" + variant.boardSize + "x" + variant.boardSize + ")"

    override def initDiffMenu: JPanel = {
      diffPanel = MenuFactory.createMenuPanel(chooseLevelString)

      panelLevelNewcomer = MenuFactory.createSubMenuLevelPanel
      panelLevelStandard = MenuFactory.createSubMenuLevelPanel
      panelLevelAdvanced = MenuFactory.createSubMenuLevelPanel

      initMenuButton(panelLevelNewcomer, MenuFactory.createLabelNewcomer,
        MenuFactory.createLevelButton(Level.Newcomer.toString), selectLevelIAListener(Level.Newcomer))
      initMenuButton(panelLevelStandard, MenuFactory.createLabelStandard,
        MenuFactory.createLevelButton(Level.Standard.toString), selectLevelIAListener(Level.Standard))
      initMenuButton(panelLevelAdvanced, MenuFactory.createLabelAdvanced,
        MenuFactory.createLevelButton(Level.Advanced.toString), selectLevelIAListener(Level.Advanced))

      initPreviousMenuButton(diffPanel, variantsPanel)

      diffPanel.addAll(Box.createRigidArea(DIMENSION_PANEL))(panelLevelNewcomer)(panelLevelStandard)(
        panelLevelAdvanced)(returnToMenu)(Box.createVerticalGlue)
      diffPanel.setVisible(false)
      diffPanel
    }

    override def initPlayerChoiceMenu: JPanel = {
      playerChoicePanel = MenuFactory.createMenuPanel(choosePlayerString)

      panelChoseWhite = MenuFactory.createSubMenuPlayerPanel
      panelChoseBlack = MenuFactory.createSubMenuPlayerPanel

      initMenuButton(panelChoseWhite, MenuFactory.createLabelWhitePlayer,
        MenuFactory.createPlayerButton(playerButtonString(Player.White)), selectPlayerListener(Player.White))
      initMenuButton(panelChoseBlack, MenuFactory.createLabelBlackPlayer,
        MenuFactory.createPlayerButton(playerButtonString(Player.Black)), selectPlayerListener(Player.Black))

      initPreviousMenuButton(playerChoicePanel, variantsPanel)

      playerChoicePanel.addAll(Box.createRigidArea(DIMENSION_PANEL))(panelChoseWhite)(panelChoseBlack)(returnToMenu)(
        Box.createVerticalGlue)
      playerChoicePanel.setVisible(false)
      playerChoicePanel
    }

    private def playerButtonString(player: Player): String = player.toString + pawnButtonString

    private def initPreviousMenuButton(previousPanel: JPanel, nextPanel: JPanel): Unit =
      initGoBackButton(previousPanel, nextPanel, previousMenuString)

    private def initGoBackButton(previousPanel: JPanel, nextPanel: JPanel, text: String): Unit = {
      returnToMenu = MenuFactory.createMainButton(text)
      returnToMenu.addActionListener((_: ActionEvent) => view.switchOverlay(previousPanel, nextPanel))
    }

    override def getBoardVariant: GameVariant = boardVariant

    override def getGameMode: GameMode = gameMode

    override def getDifficulty: Level = levelIA

    override def initInGameMenu: JPanel = {
      inGameMenuPanel = MenuFactory.createMenuPanel(chooseOptionString)
      returnToGame = MenuFactory.createMainButton(returnToGameString)
      returnToGame.addActionListener((_: ActionEvent) => view.switchOverlay(inGameMenuPanel, view.getGamePanel))

      restartGame = MenuFactory.createMainButton(restartMatchString)
      restartGame.addActionListener((_: ActionEvent) => {view.resetGUI(); view.switchOverlay(inGameMenuPanel, view.getGamePanel)})

      initGoBackButton(inGameMenuPanel, menuPanel, leaveMatchString)

      quitGame = MenuFactory.createMainButton(quitGameString)
      quitGame.addActionListener((_: ActionEvent) => System.exit(0))
      inGameMenuPanel.addAll(Box.createRigidArea(DIMENSION_PANEL))(returnToGame)(restartGame)(returnToMenu)(quitGame)(
        Box.createVerticalGlue)
      inGameMenuPanel.setVisible(false)
      inGameMenuPanel
    }

    private def selectGameModeListener(gameModeString: String): ActionListener =
      (_: ActionEvent) => {
        gameMode = GameMode.withName(gameModeString)
        view.switchOverlay(menuPanel, variantsPanel)
      }

    private def selectVariantListener(variant: GameVariant): ActionListener = (_: ActionEvent) => {
      boardVariant = variant
      gameMode match {
        case GameMode.PVP => view.switchOverlay(variantsPanel, playerChoicePanel)
        case GameMode.PVE => view.switchOverlay(variantsPanel, diffPanel)
      }
    }

    private def selectLevelIAListener(level: Level): ActionListener =
      (_: ActionEvent) => {
        levelIA = level
        view.switchOverlay(diffPanel, playerChoicePanel)
      }

    private def selectPlayerListener(player: Player): ActionListener =
      (_: ActionEvent) => {
        chosenPlayer = player
        view.resetGUI()
      }

  }
}
