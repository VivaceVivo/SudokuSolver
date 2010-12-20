package de.ptr.sudoku

import collection.SortedSet

/**
 * Field ist eines von den 81 Feldern des Sudokufeldes.
 * Jedes Field ist drei Gruppen zugeordnt: Reihe, Spalte, Block
 * Das Field speichert nur die Information über nicht zutreffende Nummern.
 * Die number wird als Option daraus berechnet und kann auch None sein.
 *
 * Created by IntelliJ IDEA.
 * User: trappp
 * Date: 20.12.2010
 * Time: 09:43:09
 */

class Field(r: Group, c: Group, b: Group) {
  var row:Group = r
  var col:Group = c
  var block:Group = b

  var nonMatching:SortedSet[Int] = SortedSet[Int]()

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
    if(number==None){
      nonMatching++= 1 to 9
      nonMatching -= num
      //println("nonMatching: " + nonMatching)
      col.propagateNumber(this)
      row.propagateNumber(this)
      block.propagateNumber(this)
    }else{
      if(number.get!=num)throw new RuntimeException()
    }
  }
  def setNotNumber(num:Int){
    if(!nonMatching.contains(num)){
      nonMatching = nonMatching + num
      if(number == None){
        col.propagateNotNumber(this, num)
        row.propagateNotNumber(this, num)
        block.propagateNotNumber(this, num)
      } else {
        col.propagateNumber(this)
        row.propagateNumber(this)
        block.propagateNumber(this)
      }
    }
  }
  def x:Int=row.fields.indexOf(this)
  def y:Int=col.fields.indexOf(this)
}