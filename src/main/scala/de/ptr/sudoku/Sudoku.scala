package de.ptr.sudoku

/**
 * A simple Sudoku solver for easy to medium difficult Sudokus
 * Solving is done using pure logic, there is no brute force
 * try and error.
 * When a Sudoku is not fully solved, you need to guess values
 * and try them manually.
 *
 * User: VivaceVivo
 * Date: 17.12.2010
 * Time: 22:43:28
 */

class Sudoku {
  val Width = 9;
  val Height = 9;
  //	val Numbers = 1 to 9;

  val cursor = new Cursor(Width, Height)

  val cols = new Array[Group](Width)
  val blocks = new Array[Group](Width)

  // initialising Groups:
  // initialising rows. Creating Fields
  val rows = Array.tabulate[Group](Height) { r =>
    val group = new Group()
    group.fields = Array.tabulate[Field](Width) { f =>
      new Field(group, cols(f), blocks(blockNrFor(f, r)))
    }
    group
  }

  // initialising columns; referencing fields from rows
  for (c <- 0 until cols.size) {
    cols(c) = new Group()
    val cFields = cols(c).fields
    for (f <- 0 until cFields.size) {
      cFields(f) = fieldInColRow(c, f)
      cFields(f).col = cols(c)
    }
  }

  // initialising blocks; referencing fields from rows
  for (b <- 0 until blocks.size) {
    blocks(b) = new Group()
    val fields = blocks(b).fields
    for (f <- 0 until fields.size) {
      val coo = coordsForBlock(b, f)
      fields(f) = fieldInColRow(coo._1, coo._2)
      fields(f).block = blocks(b)
    }
  }

  def fieldInColRow(x: Int, y: Int) = rows(y).fields(x)

  def blockNrFor(x: Int, y: Int) = x / 3 + y / Width

  def coordsForBlock(block: Int, blockPos: Int) = {
    val blockRow = block / 3
    val blocCol = block % 3
    val blockPosRow = blockPos / 3
    val blockPosCol = blockPos % 3
    (blocCol * 3 + blockPosCol, blockRow * 3 + blockPosRow)
  }

  def solved: Boolean = {
    var result = true
    rows.foreach { r =>
      result &&= r.fields.forall { f =>
        f.number != None
      }
    }
    result
  }

  def dumpSolved() {
    val x = cursor.x
    val y = cursor.y
    var c = 0;
    var r = 0;

    println("x:" + x + " y:" + y + " candidates: " + fieldInColRow(x, y).candidates.mkString(","))
    println(" -----+-----+-----")
    rows.foreach {
      row =>
        if (c == x && y == r) {
          print("(")
        } else {
          print("|")
        }
        row.fields.foreach {
          f =>
            c += 1
            val num: Option[Int] = f.number
            if (num == None) {
              printField(".", c, r)
            } else {
              printField(String.valueOf(num.get), c, r)
            }
        }
        c = 0;
        r = r + 1

        r % 3 match {
          case 0 => println; println(" -----+-----+-----")
          case _ => println
        }
    }
  }

  /**
   * zeichnet ein Feld und markiert dabei die aktuelle Position
   */
  def printField(num: String, c: Int, r: Int) {
    var blank = " ";

    c % 3 match {
      case 0 => blank = "|"
      case _ =>
    }

    if (c == cursor.x && cursor.y == r) { blank = "(" }
    if (c == cursor.x + 1 && cursor.y == r) { blank = ")" }

    print(num + blank)
  }

  def reset() {
    rows.foreach { row =>
      resetLine(row)
    }
    def resetLine(row: Group) {
      row.fields.foreach(f => f.nonMatching = f.nonMatching.take(0))
    }
  }

  def max(x: Int, y: Int): Int = if (x < y) y else x

  /**
   * Setzt alle 81 Felder auf einmal
   */
  def setNumbers(allNumbers: String) {
    assert(allNumbers.length() == 81)
    var counter = 0
    for (number <- allNumbers) {
      val row: Int = counter / 9
      val col: Int = counter % 9
      if (number.isDigit) {
        setNumber(col, row, number.asDigit)
      }
      counter += 1
    }
  }

  def setNumber(x: Int, y: Int, num: Int) {
    println("Enter " + num + " at " + x + "," + y)
    try {
      rows(y).fields(x).setNumber(num)
    } catch {
      case fex: FieldException => println(fex.getMessage())
    }
    dumpSolved()
    println
  }

}

object Sudoku {
  def main(args: Array[String]) {
    val sudoku = new Sudoku()
    edit(sudoku);
  }

  def edit(sudoku: Sudoku) {
    sudoku.dumpSolved()
    var char = -1;
    val terminal = new jline.WindowsTerminal();
    terminal.initializeTerminal();
    terminal.setDirectConsole(true);

    while (char < 0) {
      char = terminal.readCharacter(System.in);
    }

    char match {
      case 72  => sudoku.cursor.up(); sudoku.dumpSolved()
      case 80  => sudoku.cursor.down(); sudoku.dumpSolved()
      case 75  => sudoku.cursor.left(); sudoku.dumpSolved()
      case 77  => sudoku.cursor.right(); sudoku.dumpSolved()
      case 'c' => sudoku.reset(); sudoku.dumpSolved()
      case nr  => if (nr >= 48 && nr <= 57) sudoku.setNumber(sudoku.cursor.x, sudoku.cursor.y, nr - 48)
    }
    if (char != 27) { //ESC
      edit(sudoku);
    }
  }

}
