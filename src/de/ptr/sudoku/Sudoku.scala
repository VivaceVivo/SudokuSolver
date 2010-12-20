package de.ptr.sudoku


/**
 * Ein simpler Sudoku Löser für leicht bis mittlere Sudokus.
 * Derzeit werden Felder nur nach streng logischen Regeln berechnet,
 * es findet kein Ausprobieren statt.
 *
 * Created by IntelliJ IDEA.
 * User: trappp
 * Date: 17.12.2010
 * Time: 22:43:28
 */

class Sudoku {
  val rows = new Array[Group](9)
  val cols = new Array[Group](9)
  val blocks = new Array[Group](9)

  // initialisierung der Gruppen:
  // initialisierung der Reihen. Erzeugen der Felder
  for (r <- 0 until rows.size) {
    rows(r) = new Group()
    val fields = rows(r).fields
    for (f <- 0 until fields.size) {
      fields(f) = new Field(rows(r), cols(f), blocks(blockNrFor(f, r)))
    }
  }
  // Initialisieren der Spalten; die Felder aus den Reihen werden referenziert.
  for (c <- 0 until cols.size) {
    cols(c) = new Group()
    var cFields = cols(c).fields
    for (f <- 0 until cFields.size) {
      cFields(f) = fieldInColRow(c, f)
      cFields(f).col = cols(c)
    }
  }
  // Initialisieren der Bloecke; die Felder aus den Reihen werden referenziert.
  for (b <- 0 until blocks.size) {
    blocks(b) = new Group()
    var fields = blocks(b).fields
    for (f <- 0 until fields.size) {
      val coo = coordsForBlock(b, f)
      fields(f) = fieldInColRow(coo._1, coo._2)
      fields(f).block = blocks(b)
    }
  }

  def fieldInColRow(x: Int, y: Int): Field = rows(y).fields(x)

  def blockNrFor(x: Int, y: Int) = {
    var bCol = x / 3
    var bRow = y / 3
    bCol + bRow * 3
  }

  def coordsForBlock(block: Int, blockPos: Int) = {
    var blockRow = block / 3
    var blocCol = block % 3
    var blockPosRow = blockPos / 3
    var blockPosCol = blockPos % 3
    (blocCol * 3 + blockPosCol, blockRow * 3 + blockPosRow)
  }

  def dump() {
    rows.foreach {
      row =>
        row.fields.foreach {
          f =>
            val num: Option[Int] = f.number
            if (num == None) {
              print(". ")
            } else {
              print(num.get + " ")
            }
        }
        println
    }
  }

  def max(x:Int, y:Int):Int=if(x<y)y else x
  
  def dumpCandidates() {
    var len = 0
    rows.foreach{
      _.fields.foreach(f=> len = max(len, f.nonMatching.size))
    }

    rows.foreach {
      row =>
        row.fields.foreach {
          f =>
            val num: Option[Int] = f.number
            if (num == None) {
              f.nonMatching.foreach(print)
              print(" "*(len-f.nonMatching.size+1))
            } else {
              print(num.get + " "*len)
            }
        }
        println
    }
  }

  def setNumber(x: Int, y: Int, num: Int) {
    println("Enter " + num + " at " + x + "," + y)
    rows(y).fields(x).setNumber(num)
    dump()
    println
  }

}




object Sudoku {
  def main(args: Array[String]) {
    println("Enter coordinates x,y,value")
    //var row = new Group()
    //var col = new Group()
    //var block = new Group()
    var sudoku = new Sudoku()

    //    var field = new Field(sudoku.rows.head, sudoku.cols.head, sudoku.blocks.head)
    // var field = sudoku.fieldInColRow(0, 0)
    // field.nonMatching ++= List[Int](1, 2, 3, 4, 6, 5, 8, 9)

    // easy:
    //    sudoku.setNumber(0,0,8)
    //    sudoku.setNumber(1,0,6)
    //    sudoku.setNumber(2,0,1)
    //    sudoku.setNumber(4,0,4)
    //sudoku.dump
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
  //  sudoku.setNumber(1, 0, 7)

    sudoku.dumpCandidates()

  }
}
