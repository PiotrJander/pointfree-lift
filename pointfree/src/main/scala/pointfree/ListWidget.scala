package pointfree

import net.team2xh.onions.Themes
import net.team2xh.onions.components.{FramePanel, Widget}
import net.team2xh.onions.utils.{Drawing, Varying}
import net.team2xh.scurses.RichText.Foreground
import net.team2xh.scurses.{Keys, Scurses}

case class ListWidget(parent: FramePanel, items: Varying[List[String]])
                 (implicit screen: Scurses)
  extends Widget(parent) {

  override def focusable: Boolean = false

  def drawText(foreground: Int, background: Int): Unit = {
    for ((line, i) <- items.value.zipWithIndex) {
      screen.put(0, i, Drawing.clipText(line, innerWidth),
        foreground = foreground, background = background)
    }
  }

  override def redraw(focus: Boolean, theme: Themes.ColorScheme): Unit = {
    drawText(theme.foreground(focus), theme.background(focus))
  }

  override def handleKeypress(keypress: Int): Unit = {}

  override def innerHeight: Int = items.value.length
}
