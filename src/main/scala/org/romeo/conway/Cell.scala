package org.romeo.conway

sealed trait Cell {
  def nextState(liveNeighbors: Int): Cell
}

case object Alive extends Cell {
  override def nextState(liveNeighbors: Int): Cell = liveNeighbors match {
    case i if i < 2 => Dead
    case i if i > 3 => Dead
    case _ => Alive
  }
}
case object Dead extends Cell {
  override def nextState(liveNeighbors: Int): Cell = liveNeighbors match {
    case i if i == 3 => Alive
    case _ => Dead
  }
}
