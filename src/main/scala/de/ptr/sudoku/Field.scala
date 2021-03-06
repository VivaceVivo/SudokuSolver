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

  val Numbers = 1 to 9;

	val row: Group = r
  var col: Group = c
  var block: Group = b

  lazy val groups = List(row, col, block)
  var nonMatching = SortedSet[Int]()

  /**
   * erfragt, ob bereits bekannt ist, ob und welche Nummer das Feld hat.
   */
  def number: Option[Int] = {
    val numbers = candidates

    if (numbers.size == 1) {
      Some(numbers.head)
    } else {
      None
    }
  }

  /**
   * Setzt die Nummer für dieses Feld.
   */
  def setNumber(num: Int) {
    if (!number.isDefined) {
      if (nonMatching.contains(num)) {
        throw new FieldException("Number is not matching!")
      }
      nonMatching ++= (1 to 9)
      nonMatching -= num

      groups.foreach(_.propagateNumber(this, num))
    }
    if(number.isDefined){ // wegen nebenläufigkeit nicht else
      if (number.get != num) throw new FieldException("Number already set!")
    }
  }

  /**
   * Meldet, daß eine Nummer nicht in diesem Feld sein kann
   */
  def setNotNumber(num: Int) {
    if (!nonMatching.contains(num)) {
      nonMatching = nonMatching + num
      if (!number.isDefined) {
        groups.foreach(_.propagateNotNumber(this, num))
      }

      if(number.isDefined){ // wegen nebenläufigkeit nicht else
        // TODO maybe use numberoptional.foreach:
        number.foreach(n =>
          groups.foreach(_.propagateNumber(this, n))
        )
      }
    }
  }

  def x: Int = row.fields.indexOf(this)
  def y: Int = col.fields.indexOf(this)
  def candidates = Numbers.diff(nonMatching.toSeq)
  
}
