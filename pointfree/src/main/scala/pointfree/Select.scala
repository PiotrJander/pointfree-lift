package pointfree

import net.team2xh.onions.Themes
import net.team2xh.onions.components.{FramePanel, Widget}
import net.team2xh.onions.utils.{Drawing, Varying}
import net.team2xh.scurses.RichText.Foreground
import net.team2xh.scurses.{Keys, Scurses}

object Select {
  class Data(arg: List[String]) {
    var counter: Varying[Int] = 0
    var items: Varying[List[String]] = new Varying(arg)
  }
}

case class Select(parent: FramePanel, data: Select.Data)
                 (implicit screen: Scurses)
  extends Widget(parent, data.counter) {

  override def focusable: Boolean = true

  def drawText(foreground: Int, background: Int): Unit = {
    for ((line, i) <- data.items.value.zipWithIndex) {
      if (i == data.counter.value) {
        screen.put(0, i, Drawing.clipText(line, innerWidth),
          foreground = foreground, background = background)
      } else {
        screen.put(0, i, Drawing.clipText(line, innerWidth),
          foreground = background, background = foreground)
      }

    }
  }

  override def redraw(focus: Boolean, theme: Themes.ColorScheme): Unit = {
    drawText(theme.foreground(focus), theme.background(focus))
  }

  override def handleKeypress(keypress: Int): Unit = {
    if (keypress == 'j') {
      data.counter := (data.counter.value + 1) % data.items.value.length
      redraw()
    } else if (keypress == 'k') {
      data.counter := (data.counter.value - 1) % data.items.value.length
    } else {

    }
  }

  override def innerHeight: Int = data.items.value.length
}
