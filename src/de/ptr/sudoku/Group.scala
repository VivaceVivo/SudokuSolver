package de.ptr.sudoku

/**
 * Eine Gruppe ist entweder eine Reihe, Spalte oder ein Neunerblock auf dem Sudokufeld
 * Die Gruppe enthält immer 9 Felder.
 * 
 * Created by IntelliJ IDEA.
 * User: trappp
 * Date: 20.12.2010
 * Time: 09:42:29
 */

class Group {
  var fields = new Array[Field](9)

  /**
   * Die Nummer des übergebenen feldes wird in dieser Gruppe bekanntgegeben.
   * Alle anderen Felder in dieser Gruppe bekommen die Nummer als nicht zutreffend eingetragen.
   */
  def propagateNumber(field: Field){
    val num = field.number
    //println("propagateNumber: " + num)
    fields.foreach{f=>
      if(f==field){
        if(f.number != num && !(f.number==None)){
          throw new RuntimeException("falsche Zahl propagiert: "+num + " feld enthielt:"+f.number)
        }
      }else if(! (num==None)){
        f.setNotNumber(num.get)
      }
    }
  }

  /**
   * Die übergebene Nummer wird für das übergebene Feld als nicht zutreffend bekanntgegeben.
   * Es wird geprüft, ob sich daraus für diese Gruppe Rückschlüsse ziehen lassen.
   * falls für ein Feld acht nicht zutreffende Ziffern gefunden werden, wird die verbleibende
   * als Positivtreffer propagiert.
   */
  def propagateNotNumber(field: Field, num:Int){
    (1 to 9).foreach{zahl=>
      // finde das Feld in der Gruppe ohne num in nonMatching
      val matches = fields.filter{f=>
        !f.nonMatching.contains(zahl)
      }
      if(matches.length == 1){
        if(matches.head.number==None){
          matches.head.setNumber(zahl)
          println("Durch ausschluss ermittelt: " + zahl + " an Pos:"+ matches.head.x+","+ matches.head.y)
        }
      }
    }
  }
}
