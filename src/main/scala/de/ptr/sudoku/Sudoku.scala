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
	val Numbers = 1 to 9;
	
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

  def blockNrFor(x: Int, y: Int) = x/3 + y/Width

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

  def dump() { if (solved) dumpSolved(cursor.x, cursor.y) else dumpCandidates() }

  def dumpSolved() {dumpSolved(cursor.x, cursor.y)}
  
  def dumpSolved(x: Int, y: Int) {
    var c = 0;
    var r = 0;

    println("x: " + x + " y:" + y)
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
  
  def dumpCandidates() {
    var len = 0
    rows.foreach {
      _.fields.foreach(f => len = max(len, 8 - (f.nonMatching.size)))
    }

    rows.foreach {
      row =>
        row.fields.foreach {
          f =>
            val num: Option[Int] = f.number
            if (num == None) {
              val candidates = (Numbers).diff(f.nonMatching.toSeq)
              candidates.foreach(print)
              print(" " * ((len - candidates.size) + 2))
            } else {
              print(num.get + " " * (len + 1))
            }
        }
        println
    }
  }

  def reset() {
    rows.foreach { row =>
      resetLine(row)
    }
  }

  def resetLine(row: Group) {
    row.fields.foreach(f => f.nonMatching = f.nonMatching.take(0))
  }

  def max(x: Int, y: Int): Int = if (x < y) y else x

	def setNumbers() {
    
  }
  
  def setNumber(x: Int, y: Int, num: Int) {
    println("Enter " + num + " at " + x + "," + y)
    try{
    	rows(y).fields(x).setNumber(num)
    }catch{
      case fex: FieldException =>println(fex.getMessage())
    }
    dumpSolved(cursor.x, cursor.y)
    println
  }

  def readLine(row: Int, line: String): Boolean = {
    try {
      if (line.length == Width && row >= 0 && row < Height) {
        var i = 0
        line.foreach { n =>
          n match {
            case ' ' =>
            case _   => setNumber(i, row, n.asDigit)
          }
          i += 1
        }
        true
      } else {
        println("Bitte 9-Stellige Reihe eingeben (Leerzeichen für nicht besetztes Feld)")
        false
      }
    } catch {
      case e: FieldException => {
        println(e.getMessage)
        resetLine(rows(row))
        false
      }

    }
  }
}

object Sudoku {
  def main(args: Array[String]) {
    println("Enter coordinates x,y,value")
    //var row = new Group()
    //var col = new Group()
    //var block = new Group()
    val sudoku = new Sudoku()

    //    var field = new Field(sudoku.rows.head, sudoku.cols.head, sudoku.blocks.head)
    // var field = sudoku.fieldInColRow(0, 0)
    // field.nonMatching ++= List[Int](1, 2, 3, 4, 6, 5, 8, 9)

    // easy:
    //        sudoku.setNumber(0,0,8)
    //        sudoku.setNumber(1,0,6)
    //        sudoku.setNumber(2,0,1)
    //        sudoku.setNumber(4,0,4)
    //    sudoku.dump
    //    sudoku.setNumber(2,1,9)
    //    sudoku.setNumber(4,1,6)
    //    sudoku.setNumber(5,1,5)
    //    sudoku.setNumber(7,1,8)
    //sudoku.dump
    //    sudoku.setNumber(0,2,4)
    //    sudoku.setNumber(8,2,9)
    //
    //    sudoku.setNumber(1,3,7)
    //    sudoku.setNumber(3,3,8)
    //    sudoku.setNumber(5,3,2)
    //sudoku.dump
    //    sudoku.setNumber(1,4,9)
    //    sudoku.setNumber(8,4,2)
    //sudoku.dump
    //    sudoku.setNumber(0,5,2)
    //    sudoku.setNumber(1,5,4)
    //    sudoku.setNumber(2,5,8)
    //    sudoku.setNumber(3,5,5)
    //sudoku.dump
    //    sudoku.setNumber(1,6,8)
    //    sudoku.setNumber(3,6,6)
    //    sudoku.setNumber(4,6,3)
    //    sudoku.setNumber(6,6,1)
    //sudoku.dump
    //    sudoku.setNumber(0,7,5)
    //    sudoku.setNumber(2,7,7)
    //    sudoku.setNumber(3,7,2)
    //    sudoku.setNumber(6,7,3)
    //    sudoku.setNumber(7,7,6)
    //sudoku.dump
    //    sudoku.setNumber(3,8,7)
    //    sudoku.setNumber(7,8,2)

    // Schwierig
    //    sudoku.setNumber(0,0,1)
    //    sudoku.setNumber(1,0,3)
    //    sudoku.setNumber(4,0,8)
    //    sudoku.setNumber(8,0,2)
    //
    //    sudoku.setNumber(3,1,2)
    //    sudoku.setNumber(5,1,7)
    //
    //    sudoku.setNumber(2,2,6)
    //    sudoku.setNumber(6,2,8)
    //    sudoku.setNumber(7,2,3)
    //    sudoku.setNumber(8,2,7)
    //
    //    sudoku.setNumber(0,3,7)
    //    sudoku.setNumber(1,3,8)
    //    sudoku.setNumber(7,3,1)
    //
    //    sudoku.setNumber(2,4,2)
    //    sudoku.setNumber(3,4,8)
    //    sudoku.setNumber(5,4,5)
    //    sudoku.setNumber(6,4,7)
    //
    //    sudoku.setNumber(1,5,4)
    //    sudoku.setNumber(7,5,8)
    //    sudoku.setNumber(8,5,3)
    //
    //    sudoku.setNumber(0,6,8)
    //    sudoku.setNumber(1,6,1)
    //    sudoku.setNumber(2,6,9)
    //    sudoku.setNumber(6,6,3)
    //
    //    sudoku.setNumber(3,7,3)
    //    sudoku.setNumber(5,7,8)
    //
    //    sudoku.setNumber(0,8,5)
    //    sudoku.setNumber(4,8,9)
    //    sudoku.setNumber(7,8,4)
    //    sudoku.setNumber(8,8,8)
    //
    //    // guesses:
    //    sudoku.setNumber(1,4,6)

    // sehr schwierig!
    sudoku.setNumber(0, 0, 8)
    sudoku.setNumber(3, 0, 5)
    sudoku.setNumber(5, 0, 6)

    sudoku.setNumber(2, 1, 4)
    sudoku.setNumber(6, 1, 7)
    sudoku.setNumber(7, 1, 9)

    sudoku.setNumber(1, 2, 5)
    sudoku.setNumber(3, 2, 9)
    sudoku.setNumber(4, 2, 3)

    sudoku.setNumber(0, 3, 7)
    sudoku.setNumber(1, 3, 6)
    sudoku.setNumber(3, 3, 8)

    sudoku.setNumber(0, 4, 4)
    sudoku.setNumber(2, 4, 3)
    sudoku.setNumber(6, 4, 5)
    sudoku.setNumber(8, 4, 8)

    sudoku.setNumber(5, 5, 3)
    sudoku.setNumber(7, 5, 4)
    sudoku.setNumber(8, 5, 7)

    sudoku.setNumber(4, 6, 6)
    sudoku.setNumber(5, 6, 8)
    sudoku.setNumber(7, 6, 1)

    sudoku.setNumber(1, 7, 8)
    sudoku.setNumber(2, 7, 6)
    sudoku.setNumber(6, 7, 9)

    sudoku.setNumber(3, 8, 3)
    sudoku.setNumber(5, 8, 2)
    sudoku.setNumber(8, 8, 6)

    // guesses
    sudoku.setNumber(1, 0, 7)

    // aus Zeitung
    //    sudoku.readLine(0, "   82 7  ")
    //    sudoku.readLine(1, "     1 6 ")
    //    sudoku.readLine(2, "    96328")
    //    sudoku.readLine(3, "  4 1  9 ")
    //    sudoku.readLine(4, "1 9   4 2")
    //    sudoku.readLine(5, " 3  7 1  ")
    //    sudoku.readLine(6, "46358    ")
    //    sudoku.readLine(7, " 2 1     ")
    //    sudoku.readLine(8, "  5 43   ") // "  5 43   "
    //    sudoku.setNumber(8, 8, 9)
    sudoku.dump()

    // wild guess
    // sudoku.setNumber(7, 0, 1)
    
    sudoku.reset()
    sudoku.dumpSolved(sudoku.cursor.x, sudoku.cursor.y)
    edit(sudoku);

  }

  def edit(sudoku: Sudoku) {

//    sudoku.dumpSolved(sudoku.cursor.x, sudoku.cursor.y)
    var c = -1;
    val terminal = new jline.WindowsTerminal();
    terminal.initializeTerminal();
    terminal.setDirectConsole(true);

    while (c < 0) {
      c = terminal.readCharacter(System.in);
    }
    
    c match {
      case 72 => sudoku.cursor.up();sudoku.dumpSolved()
      case 80 => sudoku.cursor.down();sudoku.dumpSolved()
      case 75 => sudoku.cursor.left();sudoku.dumpSolved()
      case 77 => sudoku.cursor.right();sudoku.dumpSolved()
      case nr => if(nr>=48 && nr<=57) sudoku.setNumber(sudoku.cursor.x, sudoku.cursor.y, nr-48)
    }
    if (c != 27) { //ESC
      edit(sudoku);
    }
  }

}
