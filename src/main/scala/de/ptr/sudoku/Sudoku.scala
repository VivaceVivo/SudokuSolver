package de.ptr.sudoku

import java.io.{File, PrintWriter}
import java.util.Scanner

import scala.io.Source

/**
 * A simple Sudoku solver for easy to medium difficult Sudokus
 * Solving is done using pure logic, there is no brute force
 * try and error.
 * When a Sudoku is not fully solved, you need to guess values
 * and try them manually.
 *
 * User: VivaceVivo
 * Date: 17.12.2010
 * Time: 22:43:28
 */

abstract class Sudoku {
  val Width = 9
  val Height = 9

  val cursor = new Cursor(Width, Height)

  val cols = new Array[Group](Width)
  val blocks = new Array[Group](Width)

  var history:List[(Int, Int, Int)] = Nil

  var stepMode:Boolean = false

  def toggleStepMode = {
    stepMode = !stepMode
    println(s"stepMode is ${if(stepMode)"enabled" else "disabled"}")
  }

  // initialising Groups:
  // initialising rows. Creating Fields
  val rows = Array.tabulate[Group](Height) { r =>
    val group = new Group("r")
    group.fields = Array.tabulate[Field](Width) { f =>
      new Field(group, cols(f), blocks(blockNrFor(f, r)))
    }
    group
  }

  // initialising columns; referencing fields from rows
  for (c <- 0 until cols.size) {
    cols(c) = new Group("c")
    val cFields = cols(c).fields
    for (f <- 0 until cFields.size) {
      cFields(f) = fieldInColRow(c, f)
      cFields(f).col = cols(c)
    }
  }

  // initialising blocks; referencing fields from rows
  for (b <- 0 until blocks.size) {
    blocks(b) = new Group("b")
    val fields = blocks(b).fields
    for (f <- 0 until fields.size) {
      val coo = coordsForBlock(b, f)
      fields(f) = fieldInColRow(coo._1, coo._2)
      fields(f).block = blocks(b)
    }
  }

  def fieldInColRow(x: Int, y: Int) = rows(y).fields(x)

  def blockNrFor(x: Int, y: Int) = x / 3 + y / Width

  def coordsForBlock(block: Int, blockPos: Int) = {
    val blockRow = block / 3
    val blocCol = block % 3
    val blockPosRow = blockPos / 3
    val blockPosCol = blockPos % 3
    (blocCol * 3 + blockPosCol, blockRow * 3 + blockPosRow)
  }

  def solved: Boolean = {
    var result = true
    rows.foreach { r =>
      result &&= r.fields.forall { f =>
        f.number.isDefined
      }
    }
    result
  }

  def dumpSolved()

  def reset() {
    rows.foreach { row =>
      resetLine(row)
    }
    def resetLine(row: Group) {
      row.fields.foreach(f => f.nonMatching = f.nonMatching.take(0))
    }
    history = Nil
  }

  def max(x: Int, y: Int): Int = if (x < y) y else x

  /**
   * Setzt alle 81 Felder auf einmal
   */
  def setNumbers(allNumbers: String) {
    assert(allNumbers.length() == 81)
    var counter = 0
    for (number <- allNumbers) {
      val row: Int = counter / 9
      val col: Int = counter % 9
      if (number.isDigit) {
        setNumber(col, row, number.asDigit)
      }
      counter += 1
    }
  }

  def setNumber(x: Int, y: Int, num: Int) {
//    println("Enter " + num + " at " + x + "," + y)
    try {
      rows(y).fields(x).setNumber(num)
      history = (x, y, num)::history
    } catch {
      case fex: FieldException => println(fex.getMessage())
    }
    dumpSolved()
//    println
  }

  def querySaveTo():Option[File] = {
    println("Enter file name to store sudoku:")
    val scanner = new Scanner(System.in)
    val saveTo = scanner.nextLine()
    val resultFile = new File(saveTo)
    if(resultFile.getParentFile.isDirectory && resultFile.getParentFile.canWrite){
      Some(resultFile)
    } else {
      None
    }
  }
  def queryLoadFrom():Option[File] = {
    println("Enter file name to load sudoku from:")
    val scanner = new Scanner(System.in)
    val loadFrom = scanner.nextLine()
    val resultFile = new File(loadFrom)
    if(resultFile.isFile && resultFile.canRead){
      Some(resultFile)
    } else {
      None
    }
  }

  def undo = {
    val moves = history.drop(1).reverse
    reset()
    moves.foreach{case(x, y, num) => setNumber(x, y, num)}
  }

  def load(): Unit ={
    val loadFrom = queryLoadFrom()
    loadFrom.foreach{file =>
      reset()
      setNumbers(Source.fromFile(file).getLines().next())
    }
  }

  def save(): Unit ={
    val saveTo = querySaveTo()
    saveTo.foreach{file =>
      val fos = new PrintWriter(file)
      println("saving file to: " + file.getName)
      fos.write(serializeNumbers())
      fos.flush()
      fos.close()
    }
  }

  def serializeNumbers():String = {
    val numbers = for{
      row <- rows
      field <- row.fields
    }yield(field.number.map(String.valueOf).getOrElse(" "))
    numbers.reduce((a, b) => a+b)
  }

}

