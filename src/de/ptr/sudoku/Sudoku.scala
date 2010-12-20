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
  //rows = Array[Group].tabulate(9){i=>
  // var fields = Array[Field].tabulate(9){j=>
  //  new Field(rows, cols, blocks)
  // }
  //}
  // rows = Array[Group].tabulate(9) {i=>new Group()}
  for (r <- 0 until rows.size) {
    rows(r) = new Group()
    val fields = rows(r).fields
    for (f <- 0 until fields.size) {
      fields(f) = new Field(rows(r), cols(f), blocks(blockNrFor(f, r)))
    }
  }
  for (c <- 0 until cols.size) {
    println("col:"+c)
    cols(c) = new Group()
    println("cols(c): "+cols(c))
    var cFields = cols(c).fields
    println("fields: "+cFields)
    for (f <- 0 until cFields.size) {
      cFields(f) = fieldInColRow(c, f)
      cFields(f).theCol = cols(c)
      println("cFields(f): "+cFields(f))      
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
  println(rows)
  println(cols)
  println(blocks)
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

class Group {
  var fields = new Array[Field](9)

  def propagateNumber(field: Field){
    val num = field.number
    println("propagateNumber: " + num)
    fields.foreach{f=>
      if(f==field){
        if(f.number != num && !(f.number==None)){
          throw new RuntimeException("falsche Zahl propagiert: "+num + " feld enthielt:"+f.number)
        }
      }else{
        f.setNotNumber(num.get)
//        val oldNum = f.number
//        val nm2 = num :: f.nonMatching
//        val discovered = f.number
//        if(discovered!=None && discovered!=oldNum){
//          f.setNumber(num)
//        }// nur neu entdeckte nummer propagieren
      }
    }
  }
  def propagateNotNumber(field: Field, num:Int){
    println("propagateNotNumber")
    // TODO an dieser Stelle testen, ob aus den Ausschlussnummern in dieser Gruppe eine Positivauswahl getroffen werden kann.
     /*fields.foreach{f=>
      if(f!=field){
        f.setNotNumber(num)   
      }
     }*/
  }
}

class Field(row: Group, col: Group, block: Group) {
  var theRow:Group = row
  var theCol:Group = col
  var theBlock:Group = block
  println("row:"+theRow+" col:"+theCol+" block:"+theBlock)
  var nonMatching:Set[Int] = Set[Int]()

  def number:Option[Int] = {
    var numbers: Set[Int] = Set[Int]()
    numbers ++= 1 to 9
    numbers --= nonMatching
    if (numbers.size == 1) {
      Some(numbers.head)
    } else {
      None
    }
  }
  def setNumber(num:Int){
    nonMatching++= 1 to 9
    nonMatching -= num
    println("nonMatching: " + nonMatching)
    theCol.propagateNumber(this)
    theRow.propagateNumber(this)
    theBlock.propagateNumber(this)
  }
  def setNotNumber(num:Int){
    if(!nonMatching.contains(num)){
      nonMatching = nonMatching + num
      if(number == None){
        theCol.propagateNotNumber(this, num)
        theRow.propagateNotNumber(this, num)
        theBlock.propagateNotNumber(this, num)
      } else {
        theCol.propagateNumber(this)
        theRow.propagateNumber(this)
        theBlock.propagateNumber(this)
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
    var sudoku = new Sudoku()

    //    var field = new Field(sudoku.rows.head, sudoku.cols.head, sudoku.blocks.head)
   // var field = sudoku.fieldInColRow(0, 0)
   // field.nonMatching ++= List[Int](1, 2, 3, 4, 6, 5, 8, 9)
    sudoku.setNumber(0,0,8)
    sudoku.setNumber(1,0,6)
    sudoku.setNumber(2,0,1)
    sudoku.setNumber(4,0,4)

    sudoku.setNumber(2,1,9)
    sudoku.setNumber(4,1,6)
    sudoku.setNumber(5,1,5)
    sudoku.setNumber(8,1,8)

    sudoku.setNumber(0,2,4)
    sudoku.setNumber(8,2,9)

    sudoku.setNumber(1,3,7)
    sudoku.setNumber(3,3,8)
    sudoku.setNumber(5,3,2)

    sudoku.setNumber(1,4,9)
    sudoku.setNumber(8,4,2)

    sudoku.setNumber(0,5,2)
    sudoku.setNumber(1,5,4)
    sudoku.setNumber(2,5,8)
    sudoku.setNumber(3,5,5)

    sudoku.setNumber(1,6,8)
    sudoku.setNumber(3,6,6)
    sudoku.setNumber(4,6,3)
    sudoku.setNumber(6,6,1)

    sudoku.setNumber(0,7,5)
    sudoku.setNumber(2,7,7)
    sudoku.setNumber(3,7,2)
    sudoku.setNumber(6,7,3)
    sudoku.setNumber(7,7,6)

    sudoku.setNumber(3,8,7)
    sudoku.setNumber(7,8,2)
    
    sudoku.dump
  }
}
