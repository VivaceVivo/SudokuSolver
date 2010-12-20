package de.ptr.sudoku

/**
 * Created by IntelliJ IDEA.
 * User: trappp
 * Date: 20.12.2010
 * Time: 09:43:09
 * To change this template use File | Settings | File Templates.
 */

class Field(row: Group, col: Group, block: Group) {
  var theRow:Group = row
  var theCol:Group = col
  var theBlock:Group = block
 // println("row:"+theRow+" col:"+theCol+" block:"+theBlock)
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
        println("number: " + number)
        theCol.propagateNumber(this)
        theRow.propagateNumber(this)
        theBlock.propagateNumber(this)
      }
    }
  }
}