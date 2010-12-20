package de.ptr.sudoku

/**
 * Created by IntelliJ IDEA.
 * User: trappp
 * Date: 20.12.2010
 * Time: 09:42:29
 * To change this template use File | Settings | File Templates.
 */

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
      }
    }
  }
  def propagateNotNumber(field: Field, num:Int){
    //println("propagateNotNumber: " + num)
    // TODO an dieser Stelle testen, ob aus den Ausschlussnummern in dieser Gruppe eine Positivauswahl getroffen werden kann.
     /*fields.foreach{f=>
      if(f!=field){
        f.setNotNumber(num)
      }
     }*/
  }
}
