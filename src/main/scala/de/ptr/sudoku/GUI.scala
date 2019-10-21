package de.ptr.sudoku
import java.awt.event.KeyEvent
import java.awt.event.KeyListener

import processing.core.PApplet

class GUI extends PApplet {

  val Grid = 36

  val sudoku = new Sudoku() {
    override def dumpSolved() {
      val x = cursor.x
      val y = cursor.y
      var c = 0
      var r = 0

      paintGrid

      rows.foreach {
        row =>
          r = r + 1
          row.fields.foreach {
            f =>
              stroke(0)
              fill(0)
              c += 1
              val num: Option[Int] = f.number
              if (c - 1 == x && r - 1 == y) {
              	fill(255)
              	stroke(255, 0, 0)
              	rect(c * Grid, r * Grid, Grid - 2, Grid - 2)
              }
              if (num == None) {
              	fill(255)
              	rect(c * Grid, r * Grid, Grid - 2, Grid - 2)
              } else {
              	fill(0)
              	stroke(255)
              	text(String.valueOf(num.get), (c) * Grid + 9, (r + 1) * Grid - 4)
              }
          }
          c = 0;

      }
    }


  }
  def paintGrid {
    stroke(0)
    for (x <- 1 to 3) {
      for (y <- 1 to 3) {
        fill(0)
        rect(x * Grid * 3 - 2 * Grid - 2, y * Grid * 3 - 2 * Grid - 2, Grid * 3 + 4, Grid * 3 + 4)
        fill(255)
        rect(x * Grid * 3 - 2 * Grid + 2, y * Grid * 3 - 2 * Grid + 2, Grid * 3 - 4, Grid * 3 - 4)
      }
    }
  }

  override def setup {
    size(900, 600)
    background(255)

    addKeyListener(new KeyListener() {
      override def keyTyped(event: KeyEvent) {}
      override def keyReleased(event: KeyEvent) {}

      override def keyPressed(event: KeyEvent) {
        val keyChar = event.getKeyCode().toChar
        println("pressed: "+ keyChar)
        keyChar match {
          case KeyEvent.VK_UP    => sudoku.cursor.up()
          case KeyEvent.VK_DOWN  => sudoku.cursor.down()
          case KeyEvent.VK_LEFT  => sudoku.cursor.left()
          case KeyEvent.VK_RIGHT => sudoku.cursor.right()
          case 'S'               => sudoku.save()
          case 'L'               => sudoku.load();
          case 'C'               => sudoku.reset();
          case 'T'               => sudoku.toggleStepMode;
          case KeyEvent.VK_BACK_SPACE => sudoku.undo
          case nr                => if (nr >= 48 && nr <= 57) sudoku.setNumber(sudoku.cursor.x, sudoku.cursor.y, nr - 48)
        }

      }
    })

  }

  override def draw {
    background(255);
    val f = loadFont("AndaleMono-36.vlw")
    textFont(f, Grid)
    fill(0)
//    text("1234567890", 20, 420)
    text("x:" + sudoku.cursor.x + " y:" + sudoku.cursor.y + " - " + sudoku.fieldInColRow(sudoku.cursor.x, sudoku.cursor.y).candidates.mkString(""), 10, 26)
    sudoku.dumpSolved()
    val x = sudoku.cursor.x
    val y = sudoku.cursor.y
    text("x:" + x + " y:" + y + " candidates: " + sudoku.fieldInColRow(x, y).candidates.mkString(","), 10, 3)
  }


}

object GUI {
  def main(args: Array[String]) {
    PApplet.main(Array[String] { "--present"; "de.ptr.sudoku.GUI" })
  }
}
