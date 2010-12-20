package de.ptr.sudoku


/**
 * Created by IntelliJ IDEA.
 * User: trappp
 * Date: 17.12.2010
 * Time: 22:43:28
 * To change this template use File | Settings | File Templates.
 */

class Sudoku {
  val rows = new Array[Group](9)
  val cols = new Array[Group](9)
  val blocks = new Array[Group](9)

  // initialisierung der Gruppen:
  for (r <- 0 until rows.size) {
    rows(r) = new Group()
    val fields = rows(r).fields
    for (f <- 0 until fields.size) {
      fields(f) = new Field(rows(r), cols(f), blocks(blockNrFor(f, r)))
    }
  }
  for (c <- 0 until cols.size) {
    //println("col:"+c)
    cols(c) = new Group()
    //println("cols(c): "+cols(c))
    var cFields = cols(c).fields
    //println("fields: "+cFields)
    for (f <- 0 until cFields.size) {
      cFields(f) = fieldInColRow(c, f)
      cFields(f).theCol = cols(c)
      //println("cFields(f): "+cFields(f))
    }
  }
  for (b <- 0 until blocks.size) {
    blocks(b) = new Group()
    var fields = blocks(b).fields
    for (f <- 0 until fields.size) {
      val coo = coordsForBlock(b, f)
      fields(f) = fieldInColRow(coo._1, coo._2)
      fields(f).theBlock = blocks(b)
    }
  }
//  println(rows)
//  println(cols)
//  println(blocks)
  def fieldInColRow(x: Int, y: Int): Field = {
    rows(y).fields(x)
  }

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
  def dump(){
    rows.foreach{row=>
      row.fields.foreach{f=>
        val num:Option[Int] = f.number
        if(num==None){
          print(". ")
        } else {
          print(num.get)
          print(" ")
        }
      }
      println
    }
  }
  def setNumber(x:Int, y:Int, num:Int){
    rows(y).fields(x).setNumber(num)
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

    sudoku.setNumber(0,0,1)
    sudoku.setNumber(1,0,3)
    sudoku.setNumber(4,0,8)
    sudoku.setNumber(8,0,2)

    sudoku.setNumber(3,1,2)
    sudoku.setNumber(5,1,7)

    sudoku.setNumber(2,2,6)
    sudoku.setNumber(6,2,8)
    sudoku.setNumber(7,2,3)
    sudoku.setNumber(8,2,7)

    sudoku.setNumber(0,3,7)
    sudoku.setNumber(1,3,8)
    sudoku.setNumber(7,3,1)

    sudoku.setNumber(2,4,2)
    sudoku.setNumber(3,4,8)
    sudoku.setNumber(5,4,5)
    sudoku.setNumber(6,4,7)

    sudoku.setNumber(1,5,4)
    sudoku.setNumber(7,5,8)
    sudoku.setNumber(8,5,3)

    sudoku.setNumber(0,6,8)
    sudoku.setNumber(1,6,1)
    sudoku.setNumber(2,6,9)
    sudoku.setNumber(6,6,3)

    sudoku.setNumber(3,7,3)
    sudoku.setNumber(5,7,8)

    sudoku.setNumber(0,8,5)
    sudoku.setNumber(4,8,9)
    sudoku.setNumber(7,8,4)
    sudoku.setNumber(8,8,8)
    sudoku.dump
  }
}
