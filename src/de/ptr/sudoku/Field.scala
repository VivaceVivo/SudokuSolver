package de.ptr.sudoku

import collection.SortedSet

/**
 * This Field is one of the 81 Fields of a Sudoku puzzle.
 * Each Field belongs to three groups: row, column and block
 * A Field stores only information about numbers not matching.
 * The number of this field is calculated and is returned as Option which may be None
 *
 * User: VivaceVivo
 * Date: 20.12.2010
 * Time: 09:43:09
 */

class Field(r: Group, c: Group, b: Group) {
  val row:Group = r
  var col:Group = c
  var block:Group = b
  lazy val groups = List(row, col, block)

  var nonMatching:SortedSet[Int] = SortedSet[Int]()

  def number:Option[Int] = {
    val numbers = (1 to 9).diff(nonMatching.toSeq) 

    if (numbers.size == 1) {
      Some(numbers.head)
    } else {
      None
    }
  }

  def setNumber(num:Int){
    if(number==None){
      if(nonMatching.contains(num)){
        throw new FieldException("Number is not matching!")
      }
       nonMatching++= 1 to 9
       nonMatching -= num

      groups.foreach(_.propagateNumber(this))
    }else{
      if(number.get!=num)throw new FieldException("Number already set!")
    }
  }

  def setNotNumber(num:Int){
    if(!nonMatching.contains(num)){
      nonMatching = nonMatching + num
      if(number == None){
        groups.foreach(_.propagateNotNumber(this, num))
      } else {
        groups.foreach(_.propagateNumber(this))
      }
    }
  }
  def x:Int=row.fields.indexOf(this)
  def y:Int=col.fields.indexOf(this)
}