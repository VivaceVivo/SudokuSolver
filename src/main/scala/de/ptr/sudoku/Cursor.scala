package de.ptr.sudoku

class Cursor(width:Int, height:Int) {

  var x = 0;
  var y = 0;

  def left() { if (x > 0) x = x - 1 }
  def right() { if (x < (width-1)) x = x + 1 }
  def up() { if (y > 0) y = y - 1 }
  def down() { if (y < (height-1)) y = y + 1 }
}