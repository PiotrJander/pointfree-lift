package pointfree

import net.team2xh.onions.Themes
import net.team2xh.onions.components.{FramePanel, Widget}
import net.team2xh.onions.utils.{Drawing, Varying}
import net.team2xh.scurses.RichText.Foreground
import net.team2xh.scurses.{Keys, Scurses}

object Select {
  class Data[A](val args: Varying[List[A]], f: A => String) {

    var counter: Varying[Int] = 0
//    var items: Varying[List[String]] = new Varying(arg)

    def items: List[String] = args.value.map(f)

    args.subscribe { () =>
      counter := 0
    }
  }
}

case class Select[A](parent: FramePanel, data: Select.Data[A], action: A => Unit)
                 (implicit screen: Scurses)
  extends Widget(parent, data.counter, data.args) {

  override def focusable: Boolean = true

  def drawText(foreground: Int, background: Int): Unit = {
    for ((line, i) <- data.items.zipWithIndex) {
      if (i == data.counter.value) {
        screen.put(0, i, Drawing.clipText(line, innerWidth),
          foreground = background, background = foreground)
      } else {
        screen.put(0, i, Drawing.clipText(line, innerWidth),
          foreground = foreground, background = background)
      }

    }
  }

  override def redraw(focus: Boolean, theme: Themes.ColorScheme): Unit = {
    drawText(theme.foreground(focus), theme.background(focus))
  }

  override def handleKeypress(keypress: Int): Unit = {
    if (keypress == 'j') {
      data.counter := (data.counter.value + 1) % data.args.value.length
      redraw()
    } else if (keypress == 'k') {
      data.counter := (data.counter.value - 1) % data.args.value.length
    } else if (keypress == Keys.SPACE) {
      action(data.args.value(data.counter.value))
    }
  }

  override def innerHeight: Int = data.items.length
}
