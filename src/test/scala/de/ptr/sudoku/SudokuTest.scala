package de.ptr.sudoku

import java.util.stream.Collectors

import de.ptr.sudoku.Features.{BlockCorrelation, PropagateSpecificBlockNumber}
import org.scalatest.{BeforeAndAfter, FunSuite}

import scala.collection.JavaConverters
import scala.util.Random

class SudokuTest extends FunSuite with BeforeAndAfter {

  var sudoku:Sudoku = _

  before {
    sudoku = buildGame
  }

  test("reads numbers from string"){
    val numberstring = "       6  9 4           31  7 9          165  4    2  8 6 2            7  5      "

    sudoku.setNumbers(numberstring)

    assert(filledAreEqual(numberstring, sudoku.serializeNumbers()))
  }


  def filledAreEqual(source:String, target:String) = {
    val src = source.getBytes()
    val trg = target.getBytes()
    src.zip(trg).forall{
      case (s, t) => s==32 || s==t
    }
  }

  test("execution order makes no difference in solution"){
    val numberstring = "       6  9 4           31  7 9          165  4    2  8 6 2            7  5      "
    sudoku.setNumbers(numberstring)
    setShuffeled(sudoku.history)
    val initial = sudoku.history
    val initialSer = sudoku.serializeNumbers()
    var sameCount = 0
    var allSame = true
    println("execution order makes no difference in solution:")
    val variations = (1 to 200).map{
      index =>
        println("-------------------------------------")
        setShuffeled(initial)
        sudoku.serializeNumbers()
    }
    println(":execution order makes no difference in solution")
    val grouped = variations.groupBy(s => s)
    grouped.values.map(s => (s.head, s.length)).toList.sortBy(_._2).foreach(println)

    assert(grouped.size == 1)
  }

  private def buildGame:Sudoku = {
    new Sudoku() {
      override def dumpSolved(): Unit = {}
    }
  }

  private def setShuffeled(initial: scala.List[(Int, Int, Int)]) = {
    sudoku.reset()
    Random.shuffle(initial).foreach {
      case (x, y, num) =>
        sudoku.setNumber(x, y, num)
    }
  }

  test("solve easy sudoku completely"){
    val sudokuInput = "    3     8     1   71598    34 27  2 5   9 4  18 56    82135   1     68    8 1  "

    sudoku.setNumbers(sudokuInput)
    assert(sudoku.solved)
  }

  test("should be better or equal to me"){
    val sudokuInput = " 8     9  5 2 1         34     4 6          5    9   81  5 8   3 4               "
    val me =          "283   591459231786   859342    4 6      8   5    9   81  5 8   3 4   85          "
    sudoku.setNumbers(sudokuInput)
    assert(filledAreEqual(me, sudoku.serializeNumbers()))
    println("me: " + me)
    println("it: " + sudoku.serializeNumbers())
  }

  test("should resolve number with Block Correlation???"){
    val sudokuInput  = "        1   5  432   4  567   7        8        6        3                       "

    println("input:    " + sudokuInput)

    BlockCorrelation.enabled = false
    PropagateSpecificBlockNumber.enabled = false
    sudoku.setNumbers(sudokuInput)
    println("disabled: " + sudoku.serializeNumbers())
    assert(sudoku.serializeNumbers()(3)===' ')

    BlockCorrelation.enabled = true
    sudoku.reset()
    sudoku.setNumbers(sudokuInput)
    println("enabled:  " + sudoku.serializeNumbers())
    assert(sudoku.serializeNumbers()(3)==='2')
  }

  test("should resolve number with specific Block Correlation???"){
    val sudokuInput  = " 5     6  9 4      6    31  7 9          165  4    27 8 6 2  9         7  5      "

    println("input:    " + sudokuInput)

    PropagateSpecificBlockNumber.enabled = false
    BlockCorrelation.enabled = false
    sudoku.setNumbers(sudokuInput)
    println("disabled: " + sudoku.serializeNumbers())
    assert(sudoku.serializeNumbers()(3)===' ')

    PropagateSpecificBlockNumber.enabled = true
    sudoku.reset()
    sudoku.setNumbers(sudokuInput)
    println("enabled:  " + sudoku.serializeNumbers())
    assert(sudoku.serializeNumbers()(3)==='2')
  }
}
