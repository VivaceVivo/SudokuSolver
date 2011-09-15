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
  val cols = new Array[Group](9)
  val blocks = new Array[Group](9)

  // initialising Groups:
  // initialising rows. Creating Fields
  val rows = Array.tabulate[Group](9) { r =>
    val group = new Group()
    group.fields = Array.tabulate[Field](9) { f =>
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

  def fieldInColRow(x: Int, y: Int): Field = rows(y).fields(x)

  def blockNrFor(x: Int, y: Int) = {
    val bCol = x / 3
    val bRow = y / 3
    bCol + bRow * 3
  }

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

  def dump() { if (solved) dumpSolved() else dumpCandidates() }

  def dumpSolved() {
    var c = 0;
    println(" -----+-----+-----")
    rows.foreach {
      row =>
        print("|")
        row.fields.foreach {
          f =>
            c += 1
            val num: Option[Int] = f.number
            if (num == None) {
              c % 3 match {
              case 0 => print(".|")
              case _ => print(". ")
              }
            } else {
              c % 3 match {
                case 0 => print(num.get + "|")
                case _ => print(num.get + " ")
              }
            }
        }
        c % 27 match {
          case 0 => println; println(" -----+-----+-----")
          case _ => println
        }
    }
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
              val candidates = (1 to 9).diff(f.nonMatching.toSeq)
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

  def setNumber(x: Int, y: Int, num: Int) {
    println("Enter " + num + " at " + x + "," + y)
    rows(y).fields(x).setNumber(num)
    dumpSolved()
    println
  }

  def readLine(row: Int, line: String): Boolean = {
    try {
      if (line.length == 9 && row >= 0 && row < 9) {
        var i = 0
        line.foreach { n =>
          n match {
            case ' ' =>
            case _ => setNumber(i, row, n.asDigit)
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
//        sudoku.setNumber(1, 0, 7)

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
//    sudoku.reset()

    // wild guess
    // sudoku.setNumber(7, 0, 1)

    (0 to 8).foreach { row =>
      var processed: Boolean = false
      print("        ")
      (1 to 9).foreach(print)
      println
      while (!processed) {
        processed = sudoku.readLine(row, readLine("Zeile " + row + ":"))
      }
    }

    sudoku.dump()

  }
}
