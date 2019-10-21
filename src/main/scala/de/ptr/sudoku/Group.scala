package de.ptr.sudoku

import de.ptr.sudoku.Features.{BlockCorrelation, PropagateSpecificBlockNumber}

/**
 * A Group is one of: row, column or Block.
 * Each Group consists of exactly 9 Fields
 *
 * User: VivaceVivo
 * Date: 20.12.2010
 * Time: 09:42:29
 */

class Group(typ:String) {
  var fields = new Array[Field](9)

  val rowDefiningIndices = Set(Set(0, 1, 2), Set(3, 4, 5), Set(6, 7, 8))
  val colDefiningIndices = Set(Set(0, 3, 6), Set(1, 4, 7), Set(2, 5, 8))

  /**
   * Die Nummer des übergebenen feldes wird in dieser Gruppe bekanntgegeben.
   * Alle anderen Felder in dieser Gruppe bekommen die Nummer als nicht zutreffend eingetragen.
   */
  def propagateNumber(field: Field, num: Int) {
    fields.foreach {
      f =>
        if (f != field) {
          f.setNotNumber(num)
        }
    }
    cleanupGroup
    if(BlockCorrelation.enabled) propagateBlockRowCol
    if(PropagateSpecificBlockNumber.enabled)propagateSpecificBlockNumber
  }

  /**
   * Die übergebene Nummer wird für das übergebene Feld als nicht zutreffend bekanntgegeben.
   * Es wird geprüft, ob sich daraus für diese Gruppe Rückschlüsse ziehen lassen.
   * falls für ein Feld acht nicht zutreffende Ziffern gefunden werden, wird die verbleibende
   * als Positivtreffer propagiert.
   */
  def propagateNotNumber(field: Field, num: Int) {
    val matches = fields.filter {
      f => !f.nonMatching.contains(num)
    }

    if (matches.length == 1) {
      if (!matches.head.number.isDefined) {
        matches.head.setNumber(num)
        println("Durch ausschluss ermittelt: " + num + " an Pos:" + matches.head.x + "," + matches.head.y)
      }
    }
    cleanupGroup
    if(BlockCorrelation.enabled) propagateBlockRowCol
    if(PropagateSpecificBlockNumber.enabled)propagateSpecificBlockNumber
  }

  private def cleanupGroup = {
    // sollte eigentlich nicht nötig sein<:
    val(defined,undefined) = fields.partition(_.number.isDefined)
    val definedNumbers = defined.map(_.number.get)
    definedNumbers.foreach(defNum => undefined.foreach(u => u.setNotNumber(defNum)))
    //:>
    // cleanup Group
    val singleNumbers = fields.flatMap(_.candidates).groupBy(g => g).filter(_._2.length == 1).map(_._1)
    // TODO finde Field zu singleNumber und propagiere
    singleNumbers.foreach {
      n =>
        fields.find(f => f.candidates.contains(n)).foreach(f => f.setNumber(n))
    }
  }

  // TODO von dieser Regel gibt es noch eine speziellere Form, bei der nur eine bestimmte Zahl
  // geblockt wird
  private def propagateBlockRowCol = {
    if(typ=="b"){
      val undefined = fields.filter(f=> !f.number.isDefined)
      val countUndefined = undefined.size
      if(countUndefined==2 || countUndefined ==3){
        val undefinedNumbers = undefined.flatMap(u=>u.candidates).distinct
        if(rowDefiningIndices.exists(s => undefined.forall(u =>s.contains((u.y)*3 + u.x)))){
          println("fields:    " + fields.map(_.number.getOrElse("_")).mkString)
          println("undefined: " + undefined.map(u=>(u.y)*3 + u.x).mkString)
          println("undef_val: " + undefined.map(_.nonMatching.mkString).mkString(", "))

          undefined.head.row.fields.foreach(
            f =>
              if(!undefined.contains(f)){
                println("propagating undefined numbers as nomatch: " + undefinedNumbers.mkString)
                undefinedNumbers.foreach(f.setNotNumber(_))
              }
          )
        }
        if(colDefiningIndices.exists(s => undefined.forall(u=>s.contains(u.y*3 + u.x)))){
          undefined.head.col.fields.foreach(
            f =>
              if(!undefined.contains(f)){
                println("propagating undefined numbers as nomatch: " + undefinedNumbers.mkString)
                undefinedNumbers.foreach(f.setNotNumber(_))
//                undefined.foreach(
//                  u=>
//                    undefinedNumbers.foreach(f.setNotNumber(_))
//                )
              }
          )
        }
      }
    }
  }

  private def propagateSpecificBlockNumber = {
    if (typ == "b") {
      val undefined = fields.filter(f => !f.number.isDefined)
      val candidates = undefined
        .flatMap(u => u.candidates)
        .groupBy(c => c)
        .filter(ia => ia._2.length == 2 || ia._2.length == 3)
        .keys

      candidates.foreach{ can =>
        val specificUndefined = undefined.filter(u=>u.candidates.contains(can))
        println("s_fields:    " + fields.map(_.number.getOrElse("_")).mkString)
        println("s_undefined: " + specificUndefined.map(u => (u.y) * 3 + u.x).mkString)
        println("s_undef_val: " + specificUndefined.map(_.nonMatching.mkString).mkString(", "))

        if (rowDefiningIndices.exists(s => specificUndefined.forall(u => s.contains((u.y) * 3 + u.x)))) {
          println("s_propagating undefined r_number as nomatch: " + can)
          specificUndefined.head.row.fields.foreach(
            f =>
              if (!undefined.contains(f)) {
                f.setNotNumber(can)
              }
          )
        }
        if (colDefiningIndices.exists(s => specificUndefined.forall(u => s.contains(u.y * 3 + u.x)))) {
          println("propagating undefined c_number as nomatch: " + can)
          specificUndefined.head.col.fields.foreach(
            f =>
              if (!undefined.contains(f)) {
                    f.setNotNumber(can)
              }
          )
        }
      }
    }
  }
}
