package view.utils

import java.awt.Component

import javax.swing.JPanel

object JPanelAddAll {

  case class JPanelAdder(panel: JPanel) {
    def apply(component: Component): JPanelAdder = {
      panel.add(component)
      this
    }
  }

  implicit class ImplicitJPanelAdder(panel: JPanel) {
    implicit def addAll(component: Component): JPanelAdder = JPanelAdder(panel)(component)
  }

}
