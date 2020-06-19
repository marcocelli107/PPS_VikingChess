package view

import java.awt._
import java.awt.event.{MouseAdapter, MouseEvent}
import java.awt.image.BufferedImage
import java.io.File

import javafx.scene.media.{Media, MediaPlayer}
import javax.imageio.ImageIO
import javax.swing._
import javax.swing.border.LineBorder

trait ViewFactory {

  /**
   * Gets the smaller side of the monitor.
   *
   * @return int size
   */
  def getSmallerSide: Int

  /**
   * Creates a new Center Cell.
   *
   * cell's dimension in percent.
   *
   * @return a new Center Cell.
   */

  def createCenterCell(): Cell

  /**
   * Creates a new Corner Cell.
   *
   * @return a new Corner Cell.
   */
  def createCornerCell(): Cell

  /**
   * Creates a new Pawn Cell.
   *
   * @return a new Pawn Cell.
   */
  def createPawnCell(): Cell

  /**
   * Creates a new Normal Cell.
   *
   * @return a new Normal Cell.
   */
  def createNormalCell(): Cell

  /**
   * Creates a new BoardPanel.
   *
   * @return a new BoardPanel.
   */
  def createBoardPanel: JPanel

  /**
   * Creates a new MenuPanel.
   *
   * @param string
   * text of the menù panel.
   * @return a new MenuPanel.
   */
  def createMenuPanel(string: String): JPanel

  /**
   * Creates top or bottom panel with FlowLayout.
   *
   * @return top or bottom panel.
   *
   */
  def createTopBottomPanel: JPanel

  /**
   * Creates right or left panel with GridLayout.
   *
   * @param columns
   * number of columns.
   * @param rows
   * number of rows.
   * @return right or left panel.
   *
   */
  def createLeftRightPanel(columns: Int, rows: Int): JPanel

  /**
   * Creates a panel with OverlayLayout.
   *
   * @return a panel.
   */
  def createOverlayLayoutPanel: JPanel

  /**
   * Creates a panel with BorderLayout.
   *
   * @return a panel.
   */
  def createGamePanel: JPanel

  /**
   * Creates a panel with BoxLayout, containing board, left and right panel.
   *
   * @return a panel.
   */
  def createBoardPlusColumnsPanel: JPanel

  /**
   * Creates a menù button.
   *
   * @param string
   * name of button.
   * @return a button.
   */
  def createMenuButton(string: String): JButton

  /**
   * Creates a popup menù.
   *
   * @return a popup.
   */
  def createPopUpMenu: JPopupMenu

  /**
   * Creates an item for popup menù.
   *
   * @param text
   * text of the item.
   * @return an item.
   */
  def createJMenuItem(text: String): JMenuItem

  /**
   * Creates a game button.
   *
   * @return a button.
   */
  def createGameButton(): JButton

  /**
   * Creates a go to first move button.
   *
   * @return
   */
  def createFirstMoveButton(): JButton

  /**
   * Creates a go to previous move button.
   *
   * @return a button.
   */
  def createPreviousMoveButton(): JButton

  /**
   * Creates a go to next move button.
   *
   * @return a button.
   */
  def createNextMoveButton(): JButton

  /**
   * Creates a go to last move button.
   *
   * @return
   */
  def createLastMoveButton(): JButton

  /**
    * Creates a undo move button.
    *
    * @return a button.
    */
  def createUndoMoveButton(): JButton

  /**
   * Creates a pawn black.
   *
   * @return a label.
   */
  def createBlackPawn: JLabel

  /**
   * Creates a pawn white.
   *
   * @return a label.
   */
  def createWhitePawn: JLabel

  /**
   * Creates a king white.
   *
   * @return a label.
   */
  def createWhiteKing: JLabel

  /**
   * Creates a lost pawn black.
   *
   * @return a label.
   */
  def createLostBlackPawn: JLabel

  /**
   * Creates a lost pawn white.
   *
   * @return a label.
   */
  def createLostWhitePawn: JLabel

  /**
   * Creates a label to showing player to move and winner.
   *
   * @return a label.
   */
  def createLabelPlayerToMoveWinner: JLabel

  /**
   * Creates a frame.
   *
   * @return a frame.
   */
  def createFrame: JFrame

  /**
   * Sets the current board size to correctly define the dimension
   * of the cells.
   *
   * @author Luca Nannini
   * @param variantBoardSize
   * variant board size
   */
  def setVariantBoardSize(variantBoardSize: Int): Unit

  /**
    * Creates a sub menu panel.
    *
    * @return a panel.
    */
  def createSubMenuPanel: JPanel

  /**
    * Creates a game sub menu panel.
    *
    * @return a panel.
    */
  def createGameSubMenuPanel: JPanel

  /**
    * Creates a label for each different variant.
    *
    * @return a label.
    */
  def createLabelBoardHnefatafl: JLabel
  def createLabelBoardTawlbwrdd: JLabel
  def createLabelBoardTablut: JLabel
  def createLabelBoardBrandubh: JLabel

  /**
    * Creates a label for each different level.
    *
    * @return a label.
    */
  def createLabelNewcomer: JLabel
  def createLabelAmateur: JLabel
  def createLabelStandard: JLabel
  def createLabelAdvanced: JLabel

  /**
    * Creates a label for player white or black.
    *
    * @return a label.
    */
  def createLabelWhitePlayer: JLabel
  def createLabelBlackPlayer: JLabel

  /**
    * Generates a sound for the moved piece.
    */
  //def generateASoundForMove()
}

object ViewFactory extends ViewFactory {

  private var cellDimension = 0
  private val smallerSide: Int = ScreenSize.getSmallerSide * 9 / 10
  private val f: Font = Font.createFont(Font.TRUETYPE_FONT, new File("src/main/resources/font/NorseBold-2Kge.otf"))
  private val ge: GraphicsEnvironment = GraphicsEnvironment.getLocalGraphicsEnvironment
  ge.registerFont(f)
  private val centerCellIconPath: String = "src/main/resources/images/iconThrone.png"
  private val cornerCellIconPath: String =  "src/main/resources/images/iconCellWin.png"
  private val boardHnefataflIconPath: String =  "src/main/resources/images/iconBoardHnefatafl.png"
  private val boardTawlbwrddIconPath: String =  "src/main/resources/images/iconBoardTawlbwrdd.png"
  private val boardTablutIconPath: String =  "src/main/resources/images/iconBoardTablut.png"
  private val boardBrandubhlIconPath: String =  "src/main/resources/images/iconBoardBrandubh.png"
  private val newcomerIconPath: String =  "src/main/resources/images/iconNewcomer.png"
  private val amateurIconPath: String =  "src/main/resources/images/iconAmateur.png"
  private val standardIconPath: String =  "src/main/resources/images/iconStandard.png"
  private val advancedIconPath: String =  "src/main/resources/images/iconAdvanced.png"
  private val whitePlayerIconPath: String =  "src/main/resources/images/iconWhitePlayer.jpeg"
  private val blackPlayerIconPath: String =  "src/main/resources/images/iconBlackPlayer.jpeg"
  //private val soundMovedPiece: String =  "src/main/resources/sounds/movedPiece.mp3"

  override def getSmallerSide: Int = smallerSide

  override def createCenterCell(): Cell = centerCell()

  override def createCornerCell(): Cell = cornerCell()

  override def createPawnCell(): Cell = pawnCell()

  override def createNormalCell(): Cell = normalCell()

  override def createBoardPanel: JPanel = new BoardPanel()

  override def createMenuPanel(string: String): JPanel = new MenuPanel(string)

  override def createTopBottomPanel: JPanel = new TopBottomPanel

  override def createLeftRightPanel(columns: Int, rows: Int): JPanel = new LeftRightPanel(columns, rows)

  override def createOverlayLayoutPanel: JPanel = new OverlayLayoutPanel

  override def createGamePanel: JPanel = new GamePanel

  override def createBoardPlusColumnsPanel: JPanel = new BoardPlusColumns

  override def createMenuButton(s: String): JButton = new MenuButton(s)

  override def createJMenuItem(text: String): JMenuItem = new MenuItem(text)

  override def createPopUpMenu: JPopupMenu = new JPopupMenu

  override def createWhitePawn: JLabel = whitePawn()

  override def createBlackPawn: JLabel = blackPawn()

  override def createGameButton(): JButton = new GameButton()

  override def createFirstMoveButton(): JButton = firstMoveButton()

  override def createPreviousMoveButton(): JButton = previousMoveButton()

  override def createNextMoveButton(): JButton = nextMoveButton()

  override def createLastMoveButton(): JButton = lastMoveButton()

  override def createUndoMoveButton(): JButton = undoMoveButton()

  override def createWhiteKing: JLabel = kingPawn()

  override def createLostBlackPawn: JLabel = capturedBlackPawn()

  override def createLostWhitePawn: JLabel = capturedWhitePawn()

  override def createLabelPlayerToMoveWinner: JLabel = new LabelPlayer_Winner

  override def createFrame: JFrame = new Frame

  override def setVariantBoardSize(variantBoardSize: Int): Unit = cellDimension = smallerSide / variantBoardSize * 80 / 100

  override def createSubMenuPanel: JPanel = new SubMenuPanel

  override def createGameSubMenuPanel: JPanel = new GameSubMenuPanel

  override def createLabelBoardHnefatafl: JLabel = new IconLabel(boardHnefataflIconPath)

  override def createLabelBoardTawlbwrdd: JLabel = new IconLabel(boardTawlbwrddIconPath)

  override def createLabelBoardTablut: JLabel = new IconLabel(boardTablutIconPath)

  override def createLabelBoardBrandubh: JLabel = new IconLabel(boardBrandubhlIconPath)


  override def createLabelNewcomer: JLabel = new IconLabel(newcomerIconPath)
  override def createLabelAmateur: JLabel = new IconLabel(amateurIconPath)
  override def createLabelStandard: JLabel = new IconLabel(standardIconPath)
  override def createLabelAdvanced: JLabel = new IconLabel(advancedIconPath)


  override def createLabelWhitePlayer: JLabel = new IconLabel(whitePlayerIconPath)

  override def createLabelBlackPlayer: JLabel = new IconLabel(blackPlayerIconPath)

  //override def generateASoundForMove(): Unit = playMoveSound()

  /*
  private def playMoveSound(): Unit = {
    val mediaPlayer: MediaPlayer = new MediaPlayer(new Media(soundMovedPiece))
    mediaPlayer.play()
  }
  */

  private class BasicCell(private var defaultBackground: Color) extends Cell {
    private var isAPossibleMove: Boolean = false
    private var isLastMove: Boolean = false
    private var isKingEscapedCell: Boolean = false
    private var isSelectedCell: Boolean = false
    private var isKingCapturedCell: Boolean = false

    setPreferredSize(new Dimension(cellDimension, cellDimension))
    setAlignmentX(Component.CENTER_ALIGNMENT)
    setAlignmentY(Component.CENTER_ALIGNMENT)
    setLayout(new GridLayout())
    setBorder(new LineBorder(Color.BLACK))
    setCursor(new Cursor(Cursor.HAND_CURSOR))
    setOpaque(true)
    resetBackground()
    addMouseListener(new MouseAdapter() {
      override def mouseEntered(e: MouseEvent): Unit =
        setBackground(ColorProvider.getHighlightColor)

      override def mouseExited(e: MouseEvent): Unit = {
        if(isAPossibleMove)
          setPossibleMoveColor()
        else if(isLastMove && !isKingEscapedCell)
          setLastMoveColor()
        else if(isSelectedCell)
          setSelectedCellColor()
        else if(isKingEscapedCell)
          setKingEscapedColor()
        else if(isKingCapturedCell)
          setKingCapturedColor()
        else
          resetBackground()
      }
    })

    override def setAsPossibleMove(): Unit = {
      isAPossibleMove = true
      setPossibleMoveColor()
    }

    override def unsetAsPossibleMove(): Unit = {
      isAPossibleMove = false
      resetBackground()
    }

    override def setAsKingCapturedCell(): Unit = {
      isKingCapturedCell = true
      setKingCapturedColor()
    }

    override def resetKingCell(): Unit = {
      isKingCapturedCell = false
      isKingEscapedCell = false
      resetBackground()
    }

    override def setAsKingEscapedCell(): Unit = {
      isKingEscapedCell = true
      setKingEscapedColor()
    }

    override def setAsLastMoveCell(): Unit = {
      isLastMove = true
      if(!isKingEscapedCell)
        setLastMoveColor()
    }

    override def unsetAsLastMoveCell(): Unit = {
      isLastMove = false
      resetBackground()
    }

    override def setAsSelectedCell(): Unit = {
      isSelectedCell = true
      setSelectedCellColor()
    }

    override def unsetAsSelectedCell(): Unit = {
      isSelectedCell = false
      resetBackground()
    }

    protected def setPossibleMoveColor(): Unit = setBackground(ColorProvider.getPossibleMovesColor)

    protected def setLastMoveColor(): Unit = setBackground(ColorProvider.getLastMoveColor)

    protected def setSelectedCellColor(): Unit = setBackground(ColorProvider.getSelectedCellColor)

    private def setKingEscapedColor(): Unit = setBackground(ColorProvider.getWhiteWinColor)

    private def setKingCapturedColor(): Unit = setBackground(ColorProvider.getBlackWinColor)

    private def resetBackground(): Unit = setBackground(defaultBackground)
  }

  private class IconCell(private var defaultColor: Color, private var iconPath: String) extends BasicCell(defaultColor) {
    private var iconCell = new ImageIcon(iconPath)
    private var image = iconCell.getImage

    image = image.getScaledInstance(cellDimension * 70 / 100, cellDimension * 70 / 100, Image.SCALE_SMOOTH)
    iconCell = new ImageIcon(image)
    setIcon(iconCell)

    override def setPossibleMoveColor(): Unit = setBackground(ColorProvider.getSpecialCellPossibleMovesColor)

    override def setLastMoveColor(): Unit = setBackground(ColorProvider.getSpecialCellLastMoveColor)

    override def setSelectedCellColor(): Unit = setBackground(ColorProvider.getSpecialCellSelectedCellColor)

  }

  private def pawnCell(): Cell = new BasicCell(ColorProvider.getPawnCellColor)
  private def normalCell(): Cell = new BasicCell(ColorProvider.getNormalCellColor)
  private def centerCell(): Cell = new IconCell(ColorProvider.getSpecialCellColor, centerCellIconPath)
  private def cornerCell(): Cell = new IconCell(ColorProvider.getSpecialCellColor, cornerCellIconPath)

  private class BoardPanel extends JPanel {
    this.setBackground(ColorProvider.getLightBrownColor)
  }

  private class MenuPanel(string: String) extends JPanel {

    private val menuLabel = new JLabel()

    private val chooseLabel = new JLabel()

    private val image = ImageIO.read(new File("src/main/resources/images/Cornice.png"))

    private val imageScaled = image.getScaledInstance(smallerSide, smallerSide * 98 / 100, Image.SCALE_DEFAULT)

    private val img = new ImageIcon("src/main/resources/images/logo.png")

    setLayout(new BoxLayout(this, BoxLayout.Y_AXIS))
    menuLabel.setPreferredSize(new Dimension(smallerSide, smallerSide * 25 / 100))
    menuLabel.setAlignmentX(Component.CENTER_ALIGNMENT)
    menuLabel.setIcon(img)
    chooseLabel.setPreferredSize(new Dimension(smallerSide, smallerSide * 6 / 100))
    chooseLabel.setAlignmentX(Component.CENTER_ALIGNMENT)
    chooseLabel.setFont(new Font(f.getFontName, Font.BOLD, smallerSide * 6 / 100))
    chooseLabel.setText(string)
    chooseLabel.setForeground(ColorProvider.getWhiteColor)
    add(Box.createRigidArea(new Dimension(smallerSide, smallerSide * 5 / 100)))
    add(menuLabel)
    add(Box.createRigidArea(new Dimension(smallerSide, smallerSide * 1 / 100)))
    add(chooseLabel)
    add(Box.createRigidArea(new Dimension(smallerSide, smallerSide * 1 / 100)))


    override protected def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      g.drawImage(imageScaled, 0, 0, null)
    }
  }

  private class TopBottomPanel extends JPanel {
    private val WIDTH_DIMENSION = smallerSide
    private val HEIGHT_DIMENSION = smallerSide * 8 / 100
    setLayout(new BoxLayout(this, BoxLayout.X_AXIS))
    setBackground(ColorProvider.getLightBrownColor)
    setPreferredSize(new Dimension(WIDTH_DIMENSION, HEIGHT_DIMENSION))
  }

  private class LeftRightPanel(val columns: Int, val rows: Int) extends JPanel {
    private val WIDTH_DIMENSION = smallerSide * 10 / 100
    private val HEIGHT_DIMENSION = smallerSide * 80 / 100
    private val colorSide = ColorProvider.getLightBrownColor
    setLayout(new GridLayout(rows, columns))
    setBackground(colorSide)
    setPreferredSize(new Dimension(WIDTH_DIMENSION, HEIGHT_DIMENSION))
  }

  private class OverlayLayoutPanel extends JPanel {
    setLayout(new OverlayLayout(this))
    setVisible(true)
  }

  private class GamePanel extends JPanel {
    setBackground(ColorProvider.getLightBrownColor)
  }

  private class BoardPlusColumns extends JPanel {
    setLayout(new BoxLayout(this, BoxLayout.X_AXIS))
  }

  abstract private class EmptyButton(val s: String) extends JButton {
    setCursor(new Cursor(Cursor.HAND_CURSOR))
    setHorizontalAlignment(SwingConstants.CENTER)
    setText(s)
  }

  private class MenuButton(s: String) extends EmptyButton(s) {

    private val FONT_DIMENSION = smallerSide * 5 / 100
    //private val BUTTON_DIMENSION = new Dimension(smallerSide * 60 / 100, smallerSide * 8 / 100)
    private val BUTTON_DIMENSION = new Dimension(smallerSide * 4 / 10, smallerSide * 8 / 100)

    setPreferredSize(BUTTON_DIMENSION)
    setMaximumSize(getPreferredSize)
    setAlignmentX(Component.CENTER_ALIGNMENT)

    setOpaque(false)
    setContentAreaFilled(false)
    setBorderPainted(false)

    setFont(new Font(f.getFontName, Font.BOLD, FONT_DIMENSION))
    setForeground(ColorProvider.getWhiteColor)
    setBorderPainted(false)

    addMouseListener(new MouseAdapter() {
      override def mouseEntered(e: MouseEvent): Unit = {
        setForeground(ColorProvider.getNormalCellColor)
      }

      override def mouseExited(e: MouseEvent): Unit = {
        setForeground(ColorProvider.getWhiteColor)
      }
    })
  }

  private class IconLabel(pathIcon: String) extends JLabel {
    //private val ICON_DIMENSION: Int = smallerSide * 7 / 100
    private val ICON_DIMENSION: Int = smallerSide * 49 / 1000
    this.setPreferredSize(new Dimension(ICON_DIMENSION, ICON_DIMENSION))
    this.setMaximumSize(getPreferredSize)
    val img: BufferedImage = ImageIO.read(new File(pathIcon))
    val dimg: Image = img.getScaledInstance(ICON_DIMENSION, ICON_DIMENSION, Image.SCALE_SMOOTH)
    val imageIcon = new ImageIcon(dimg)
    this.setIcon(imageIcon)
  }

  private class SubMenuPanel extends JPanel {
    //private val PANEL_DIMENSION = new Dimension(smallerSide * 45 / 100, smallerSide * 8 / 100)
    private val PANEL_DIMENSION = new Dimension(smallerSide * 7 / 10, smallerSide * 8 / 100)

    setPreferredSize(PANEL_DIMENSION)
    setMaximumSize(getPreferredSize)
    setAlignmentX(Component.CENTER_ALIGNMENT)
    setOpaque(false)
    setVisible(true)

    val gridBagLayout: GridBagLayout = new java.awt.GridBagLayout()
    this.setLayout(gridBagLayout)
  }

  private class GameSubMenuPanel extends JPanel {
    private val PANEL_DIMENSION = new Dimension(smallerSide * 45 / 100, smallerSide * 8 / 100)

    setPreferredSize(PANEL_DIMENSION)
    setMaximumSize(getPreferredSize)
    setOpaque(false)
    setVisible(true)

    val gridBagLayout: GridBagLayout = new java.awt.GridBagLayout()
    this.setLayout(gridBagLayout)
  }

  private class GameButton() extends EmptyButton("") {
    private var imageIcon = new ImageIcon("src/main/resources/images/hamburgerMenu.png")
    private var image = imageIcon.getImage
    image = image.getScaledInstance(smallerSide * 7 / 100, smallerSide * 7 / 100, Image.SCALE_SMOOTH)
    imageIcon = new ImageIcon(image)
    setIcon(imageIcon)
    setToolTipText("Game Menu")
    setBorderPainted(false)
    setOpaque(false)
    setContentAreaFilled(false)
  }

  private class SnapshotButton(private val iconPath: String, private val hoverText: String) extends EmptyButton("") {
    private var imageIcon = new ImageIcon(iconPath)
    private var image = imageIcon.getImage
    image = image.getScaledInstance(smallerSide * 5 / 100, smallerSide * 5 / 100, Image.SCALE_SMOOTH)
    imageIcon = new ImageIcon(image)
    setIcon(imageIcon)
    setToolTipText(hoverText)
    setBorderPainted(false)
    setOpaque(false)
    setContentAreaFilled(false)
  }

  private val firstMoveButtonPath: String = "src/main/resources/images/iconFirstMove.png"
  private val firstMoveButtonHoverText: String = "Show First Move"
  private def firstMoveButton(): JButton = new SnapshotButton(firstMoveButtonPath, firstMoveButtonHoverText)

  private val previousMoveButtonPath: String = "src/main/resources/images/iconPreviousMove.png"
  private val previousMoveButtonHoverText: String = "Show Previous Move"
  private def previousMoveButton(): JButton = new SnapshotButton(previousMoveButtonPath, previousMoveButtonHoverText)

  private val nextMoveButtonPath: String = "src/main/resources/images/iconNextMove.png"
  private val nextMoveButtonHoverText: String = "Show Next Move"
  private def nextMoveButton(): JButton = new SnapshotButton(nextMoveButtonPath, nextMoveButtonHoverText)

  private val lastMoveButtonPath: String = "src/main/resources/images/iconLastMove.png"
  private val lastMoveButtonHoverText: String = "Show Last Move"
  private def lastMoveButton(): JButton = new SnapshotButton(lastMoveButtonPath, lastMoveButtonHoverText)

  private val undoMoveButtonPath: String = "src/main/resources/images/iconUndoMove.png"
  private val undoMoveButtonHoverText: String = "Turn Back"
  private def undoMoveButton(): JButton = new SnapshotButton(undoMoveButtonPath, undoMoveButtonHoverText)

  private class PopUpMenu extends JPopupMenu {

    private val FONT_DIMENSION = smallerSide * 3 / 100
    setCursor(new Cursor(Cursor.HAND_CURSOR))
    setFont(new Font(f.getFontName, Font.PLAIN, FONT_DIMENSION))

  }

  private class MenuItem(text: String) extends JMenuItem {

    private val colorSide = ColorProvider.getBrownColor
    private val FONT_DIMENSION = smallerSide * 3 / 100

    setText(text)
    setFont(new Font(f.getFontName, Font.PLAIN, FONT_DIMENSION))
    setForeground(Color.WHITE)
    setBackground(colorSide)
    setOpaque(true)

  }

  private class Pawn(private val internalColor: Color, private val externalColor: Color,
              private val sizeMultiplier: Double) extends JLabel {
    private val EXTERNAL_ROUNDRECT_MULTIPLIER: Int = (cellDimension * 8 / 10 * sizeMultiplier).toInt
    private val INTERNAL_ROUNDRECT_MULTIPLIER: Int = (cellDimension * 7 / 10 * sizeMultiplier).toInt
    private val ARC_DIMENSION: Int = 10

    setOpaque(false)
    setVisible(true)

    override def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      val X_CENTRE = getWidth / 2
      val Y_CENTRE = getHeight / 2
      val radius1 = EXTERNAL_ROUNDRECT_MULTIPLIER * Math.sqrt(2).toInt / 2
      val radius2 = INTERNAL_ROUNDRECT_MULTIPLIER * Math.sqrt(2).toInt / 2
      g.setColor(externalColor)
      g.fillRoundRect(X_CENTRE - radius1, Y_CENTRE - radius1, EXTERNAL_ROUNDRECT_MULTIPLIER, EXTERNAL_ROUNDRECT_MULTIPLIER, ARC_DIMENSION, ARC_DIMENSION)
      g.setColor(internalColor)
      g.fillRoundRect(X_CENTRE - radius2, Y_CENTRE - radius2, INTERNAL_ROUNDRECT_MULTIPLIER, INTERNAL_ROUNDRECT_MULTIPLIER, ARC_DIMENSION, ARC_DIMENSION)
    }
  }

  private val basicPawnSizeMultiplier: Double = 1
  private val capturedPawnSizeMultiplier: Double = 0.5
  private def whitePawn(): JLabel = new Pawn(ColorProvider.getWhiteColor, ColorProvider.getBlackColor, basicPawnSizeMultiplier)
  private def blackPawn(): JLabel = new Pawn(ColorProvider.getBlackColor, ColorProvider.getWhiteColor, basicPawnSizeMultiplier)
  private def kingPawn(): JLabel = new Pawn(ColorProvider.getWhiteColor, ColorProvider.getGoldColor, basicPawnSizeMultiplier)
  private def capturedWhitePawn(): JLabel = new Pawn(ColorProvider.getWhiteColor, ColorProvider.getBlackColor, capturedPawnSizeMultiplier)
  private def capturedBlackPawn(): JLabel = new Pawn(ColorProvider.getBlackColor, ColorProvider.getWhiteColor, capturedPawnSizeMultiplier)

  private class LabelPlayer_Winner extends JLabel {
    private val DIMENSION_FONT: Int = smallerSide * 3 / 100
    setFont(new Font(f.getFontName, Font.BOLD, DIMENSION_FONT))
    setForeground(ColorProvider.getPossibleMovesColor)
  }

  private class Frame extends JFrame {
    private val FRAME_TITLE = "Viking Chess - Hnefatafl"
    private val iconApp = new ImageIcon("src/main/resources/images/iconApp.png")

    setTitle(FRAME_TITLE)
    setIconImage(iconApp.getImage)
    setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    setResizable(false)
    setSize(smallerSide, smallerSide)
    setLocationRelativeTo(null)
  }
}
