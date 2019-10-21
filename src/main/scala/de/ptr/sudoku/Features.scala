package de.ptr.sudoku

object Features {
  class Feature(var enabled:Boolean=true)

  case object BlockCorrelation extends Feature
  case object PropagateSpecificBlockNumber extends Feature(true)
}
