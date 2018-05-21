package org.romeo.conway

trait GameOfLife {
  def iterate(): GameOfLife
  def cellAt(row: Int, column: Int): Cell
}
