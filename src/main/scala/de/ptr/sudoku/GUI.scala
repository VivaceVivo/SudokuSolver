package de.ptr.sudoku
import java.awt.event.KeyEvent
import java.awt.event.KeyListener
import java.lang.String

import processing.core.PApplet

class GUI extends PApplet {

  val sudoku = new Sudoku() {
    
    override def dumpSolved() {
      val x = cursor.x
      val y = cursor.y
      var c = 0;
      var r = 0;

      text("x:" + x + " y:" + y + " candidates: " + fieldInColRow(x, y).candidates.mkString(","), 10, 30)
      rows.foreach {
        row =>
          r = r + 1
          row.fields.foreach {
            f =>
              c += 1
              val num: Option[Int] = f.number
              if (num != None) {
                text(String.valueOf(num.get), c * 29, r * 29)
              }else{
                text(".", c * 29, r * 29)
              }
          }
          c = 0;

      }
    }

  }

  override def setup {
    size(700, 400);
    background(255);

    addKeyListener(new KeyListener() {
      override def keyTyped(event: KeyEvent) {
      }
      override def keyReleased(event: KeyEvent) {

      }
      override def keyPressed(event: KeyEvent) {

        event.getKeyCode() match {
          case KeyEvent.VK_UP    => sudoku.cursor.up();
          case KeyEvent.VK_DOWN  => sudoku.cursor.down();
          case KeyEvent.VK_LEFT  => sudoku.cursor.left();
          case KeyEvent.VK_RIGHT => sudoku.cursor.right();
          case 'c'               => sudoku.reset();
          case nr                => if (nr >= 48 && nr <= 57) sudoku.setNumber(sudoku.cursor.x, sudoku.cursor.y, nr - 48)

        }

      }
    })

  }

  override def draw {
    //    stroke(0);
    //    if (mousePressed) {
    //      line(mouseX, mouseY, pmouseX, pmouseY);
    //      ellipse(mouseX, mouseY, pmouseX, pmouseY);
    //    }
    //    val f = loadFont("Ziggurat-48.vlw");
    background(255);
    val f = loadFont("AndaleMono-36.vlw");
    textFont(f, 36);
    fill(0);
    //    text("1234567890", 10, 100);
    sudoku.dumpSolved()
  }

  object GUI {
    def main(args: Array[String]) {
      PApplet.main(Array[String] { "--present"; "de.ptr.sudoku.GUI" });
    }
  }
}