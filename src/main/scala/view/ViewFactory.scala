package view

import java.awt._
import java.awt.event.{MouseAdapter, MouseEvent}
import java.io.File

import javax.imageio.ImageIO
import javax.swing._
import javax.swing.border.LineBorder

/**
 * @author Marco Celli
 * @author Pasquale Leo
 */

trait ViewFactory {

  def getSmallerSide: Int

  /**
   * Creates a new Center Cell.
   *
   * cell's dimension in percent.
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
   * Creates a new GamePanel.
   *
   * column of GridLayout.
   * row of GridLayout.
   *
   * @return a new GamePanel.
   */
  def createBoardPanel: JPanel

  /**
   * Creates a new MenuPanel.
   *
   * @return a new MenuPanel.
   */
  def createMenuPanel(string: String): JPanel


  /**
   * Creates a panel with flow layout.
   *
   * @return a panel with flow layout.
   *
   */
  def createTopBottomPanel: JPanel

  def createLeftRightPanel(columns: Int, rows: Int): JPanel

  /**
   * Creates a panel with overlay layout.
   *
   * @return a panel with overlay layout.
   *
   */
  def createOverlayLayoutPanel: JPanel

  /**
   * Creates a panel with border layout.
   *
   * @return a panel with border layout.
   *
   */
  def createGamePanel: JPanel

  def createBoardPlusColumnsPanel: JPanel

  /**
   * Creates a button with black background.
   * * @param s
   * name of button.
   *
   * @return a panel with border layout.
   *
   */
  def createMenuButton(s: String): JButton

  def createPopUpButton: JPopupMenu

  def createJMenuItem(text: String): JMenuItem

  /**
   * Creates a button.
   * @param s
   * name of button.
   *
   * @return a panel with border layout.
   *
   */
  def createGameButton(s: String): JButton

  /**
   * Creates a black pawn.
   *
   * @return a label.
   *
   */
  def createBlackPawn: JLabel

  /**
   * Creates a white pawn.
   *
   * @return a label.
   *
   */
  def createWhitePawn: JLabel

  /**
   * Creates a king pawn.
   *
   * @return a label.
   *
   */
  def createWhiteKing: JLabel

  def createLostBlackPawn: JLabel


  def createLostWhitePawn: JLabel


  /**
   * Creates a Winner Label.
   *
   * @return a JLabel.
   *
   */
  def createLabelPlayerToMoveWinner: JLabel

  /**
   * Creates a Frame.
   *
   * @return a JFrame.
   *
   */
  def createFrame: JFrame

  /**
   * Sets the current board size to correctly define the dimension
   * of the cells.
   *
   * @author Luca Nannini
   * @param variantBoardSize
   *                         variant board size
   */
  def setVariantBoardSize(variantBoardSize: Int): Unit
}

object ViewFactory extends ViewFactory {

  private var cellDimension = 0
  var smallerSide: Int = ScreenSize.getSmallerSide * 9 / 10
  val f: Font = Font.createFont(Font.TRUETYPE_FONT, new File("src/main/resources/font/NorseBold-2Kge.otf"))
  val ge: GraphicsEnvironment = GraphicsEnvironment.getLocalGraphicsEnvironment
  ge.registerFont(f)

  override def getSmallerSide: Int = smallerSide

  override def createCenterCell(): Cell = new CenterCell()

  override def createCornerCell(): Cell = new CornerCell()

  override def createPawnCell(): Cell = new PawnCell()

  override def createNormalCell(): Cell = new NormalCell()

  override def createBoardPanel: JPanel = new BoardPanel()

  override def createMenuPanel(string: String): JPanel = new MenuPanel(string)

  override def createTopBottomPanel: JPanel = new TopBottomPanel

  override def createLeftRightPanel(columns: Int, rows: Int): JPanel = new LeftRightPanel(columns, rows)

  override def createOverlayLayoutPanel: JPanel = new OverlayLayoutPanel

  override def createGamePanel: JPanel = new GamePanel

  override def createBoardPlusColumnsPanel: JPanel = new BoardPlusColumns

  override def createMenuButton(s: String): JButton = new MenuButton(s)

  override def createPopUpButton: JPopupMenu = new JPopupMenu

  override def createJMenuItem(text: String): JMenuItem = new MenuItem(text)

  override def createGameButton(s: String): JButton = new GameButton(s)

  override def createWhitePawn: JLabel = new WhitePawn

  override def createBlackPawn: JLabel = new BlackPawn

  override def createWhiteKing: JLabel = new KingPawn

  override def createLostBlackPawn: JLabel = new LostBlackPawn

  override def createLostWhitePawn: JLabel = new LostWhitePawn

  override def createLabelPlayerToMoveWinner: JLabel = new LabelPlayer_Winner

  override def createFrame: JFrame = new Frame

  override def setVariantBoardSize(variantBoardSize: Int): Unit = cellDimension = smallerSide / variantBoardSize * 80 / 100

  abstract class AbstractCell extends Cell {
    private var isAPossibleMove: Boolean = false
    private var isLastMove: Boolean = false
    protected var defaultBackground: Color = _


    setPreferredSize(new Dimension(cellDimension, cellDimension))
    setAlignmentX(Component.CENTER_ALIGNMENT)
    setAlignmentY(Component.CENTER_ALIGNMENT)
    setLayout(new GridLayout())
    setBorder(new LineBorder(Color.BLACK))
    setCursor(new Cursor(Cursor.HAND_CURSOR))
    setOpaque(true)
    addMouseListener(new MouseAdapter() {
      override def mouseEntered(e: MouseEvent): Unit =
        setBackground(ColorProvider.getHighlightColor)

      override def mouseExited(e: MouseEvent): Unit = {
        if(isAPossibleMove)
          setBackground(ColorProvider.getPossibleMovesColor)
        else if(isLastMove)
          setBackground(ColorProvider.getLastMoveColor)
        else
          resetBackground()
      }
    })

    protected def resetBackground(): Unit = setBackground(defaultBackground)

    override def setAsPossibleMove(): Unit = {
      isAPossibleMove = true
      setBackground(ColorProvider.getPossibleMovesColor)
    }

    override def unsetAsPossibleMove(): Unit = {
      isAPossibleMove = false
      resetBackground()
    }

    override def setAsKingCapturedCell(): Unit = {
      defaultBackground = ColorProvider.getBlackWinColor
      resetBackground()
    }

    override def setAsKingEscapedCell(): Unit = {
      defaultBackground = ColorProvider.getWhiteWinColor
      resetBackground()
    }

    override def setAsLastMoveCell(): Unit = {
      isLastMove = true
      setBackground(ColorProvider.getLastMoveColor)
    }

    override def unsetAsLastMoveCell(): Unit = {
      isLastMove = false
      resetBackground()
    }
  }

  private abstract class SpecialCell() extends AbstractCell {
    defaultBackground = ColorProvider.getSpecialCellColor
    resetBackground()
  }

  private class CenterCell() extends SpecialCell {

    private var iconCell = new ImageIcon("src/main/resources/images/iconThrone.png")
    private var image = iconCell.getImage

    image = image.getScaledInstance(cellDimension * 70 / 100, cellDimension * 70 / 100, Image.SCALE_SMOOTH)

    iconCell = new ImageIcon(image)

    setIcon(iconCell)

  }

  private class CornerCell() extends SpecialCell {

    private var iconCell = new ImageIcon("src/main/resources/images/iconCellWin.png")
    private var image = iconCell.getImage

    image = image.getScaledInstance(cellDimension * 70 / 100, cellDimension * 70 / 100, Image.SCALE_SMOOTH)

    iconCell = new ImageIcon(image)

    setIcon(iconCell)

  }

  private class PawnCell() extends AbstractCell {
    defaultBackground = ColorProvider.getPawnCellColor
    resetBackground()
  }

  private class NormalCell() extends AbstractCell {
    defaultBackground = ColorProvider.getNormalCellColor
    resetBackground()
  }

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

    setBackground(ColorProvider.getLightBrownColor)
    setPreferredSize(new Dimension(WIDTH_DIMENSION, HEIGHT_DIMENSION))
    setAlignmentX(Component.RIGHT_ALIGNMENT)

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

    private val FONT_DIMENSION = smallerSide * 6 / 100
    private val BUTTON_DIMENSION = new Dimension(smallerSide * 60 / 100, smallerSide * 10 / 100)

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

  private class GameButton(s: String) extends EmptyButton(s) {
    private var imageIcon = new ImageIcon("src/main/resources/images/hamburgerMenu.png")
    private var image = imageIcon.getImage
    image = image.getScaledInstance(smallerSide * 7 / 100, smallerSide * 7 / 100, Image.SCALE_SMOOTH)
    imageIcon = new ImageIcon(image)
    setIcon(imageIcon)
    setBorderPainted(false)
    setOpaque(false)
    setContentAreaFilled(false)

  }

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

  abstract private class Pawn extends JLabel {

    protected var externalColor: Color = _
    protected var internalColor: Color = _
    protected var namePawn: String = _

    protected var EXTERNAL_ROUNDRECT_DIMENSION: Int = cellDimension * 8 / 10
    protected var INTERNAL_ROUNDRECT_DIMENSION: Int = cellDimension * 7 / 10

    setOpaque(false)
    setVisible(true)


    override def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      val X_CENTRE = getWidth / 2
      val Y_CENTRE = getHeight / 2
      val radius1 = EXTERNAL_ROUNDRECT_DIMENSION * Math.sqrt(2).toInt / 2
      val radius2 = INTERNAL_ROUNDRECT_DIMENSION * Math.sqrt(2).toInt / 2
      g.setColor(externalColor)
      g.fillRoundRect(X_CENTRE - radius1, Y_CENTRE - radius1, EXTERNAL_ROUNDRECT_DIMENSION, EXTERNAL_ROUNDRECT_DIMENSION, 10, 10)
      g.setColor(internalColor)
      g.fillRoundRect(X_CENTRE - radius2, Y_CENTRE - radius2, INTERNAL_ROUNDRECT_DIMENSION, INTERNAL_ROUNDRECT_DIMENSION, 10, 10)
    }

  }

  private class WhitePawn extends Pawn {
    namePawn = "white"
    externalColor = ColorProvider.getBlackColor
    internalColor = ColorProvider.getWhiteColor
  }

  private class BlackPawn extends Pawn {
    namePawn = "black"
    externalColor = ColorProvider.getWhiteColor
    internalColor = ColorProvider.getBlackColor
  }

  private class KingPawn extends Pawn {
    namePawn = "king"
    externalColor = ColorProvider.getGoldColor
    internalColor = ColorProvider.getWhiteColor

  }

  private class LostWhitePawn extends WhitePawn {
    EXTERNAL_ROUNDRECT_DIMENSION = EXTERNAL_ROUNDRECT_DIMENSION / 2
    INTERNAL_ROUNDRECT_DIMENSION = INTERNAL_ROUNDRECT_DIMENSION / 2
  }

  private class LostBlackPawn extends BlackPawn {
    EXTERNAL_ROUNDRECT_DIMENSION = EXTERNAL_ROUNDRECT_DIMENSION / 2
    INTERNAL_ROUNDRECT_DIMENSION = INTERNAL_ROUNDRECT_DIMENSION / 2
  }

  private class LabelPlayer_Winner extends JLabel {
    private val DIMENSION_FONT: Int = smallerSide * 7 / 100
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
