package view

import java.awt.{Dimension, Toolkit}

/**
 * A class for providing informations about screen size.
 *
 */
object ScreenSize {

    private var smallerSide = 0

    val screen: Dimension = Toolkit.getDefaultToolkit.getScreenSize
    /*
    smallerSide = (if (screen.getHeight < screen.getWidth) screen.getHeight
    else screen.getWidth).toInt
    */

    /** The screen size is equal to the smaller size of the monitor. */
    screen match {
        case height if screen.getHeight < screen.getWidth => smallerSide = screen.getHeight.toInt
        case width if screen.getHeight > screen.getWidth => smallerSide = screen.getWidth.toInt
    }

    /**
      * Returns the size of the smaller side of the screen.
      *
      * @return the size of the smaller side of the screen.
      */
    def getSmallerSide: Int = smallerSide
}


