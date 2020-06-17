package view

import java.awt.Color

trait ColorProvider {

  def getBlackColor: Color

  def getHighlightColor: Color

  def getSpecialCellColor: Color

  def getPawnCellColor: Color

  def getNormalCellColor: Color

  def getPossibleMovesColor: Color

  def getSpecialCellPossibleMovesColor: Color

  def getLightBrownColor: Color

  def getBrownColor: Color

  def getWhiteColor: Color

  def getGoldColor: Color

  def getBlackWinColor: Color

  def getWhiteWinColor: Color

  def getLastMoveColor: Color

  def getSpecialCellLastMoveColor: Color

  def getSelectedCellColor: Color

  def getSpecialCellSelectedCellColor: Color
}

object ColorProvider extends ColorProvider {

  override def getBlackColor: Color = Color.BLACK

  override def getHighlightColor: Color = Color.LIGHT_GRAY

  override def getSpecialCellColor: Color = new Color(46, 50, 100)

  override def getPawnCellColor: Color = new Color(153, 203, 205)

  override def getNormalCellColor: Color = new Color(83, 143, 159)

  override def getPossibleMovesColor: Color =  new Color(52, 89, 99)//new Color(41, 71, 79)

  override def getSpecialCellPossibleMovesColor: Color = new Color(34, 57, 64)

  override def getLightBrownColor: Color = new Color(200, 170, 109)

  override def getBrownColor: Color = new Color(114, 73, 51)

  override def getWhiteColor: Color = new Color(255, 250, 240)

  override def getGoldColor: Color = new Color(212, 175, 55)

  override def getBlackWinColor: Color = Color.decode("#953e3e")//new Color(203, 50, 52)

  override def getWhiteWinColor: Color = Color.decode("#5a7a34")//new Color(119, 221, 119)

  override def getLastMoveColor: Color = Color.decode("#cdd26a")//new Color(172, 209, 8)

  override def getSpecialCellLastMoveColor: Color = Color.decode("#597502")

  override def getSelectedCellColor: Color = Color.decode("#829769")//new Color(71, 145, 31)

  override def getSpecialCellSelectedCellColor: Color = Color.decode("#646f40")

}
