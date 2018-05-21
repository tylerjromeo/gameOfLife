package org.romeo.conway


class Grid(grid: Seq[Seq[Cell]]) extends GameOfLife {


  override def iterate(): GameOfLife = new Grid(grid.zipWithIndex.map{
    case (cs, rowNum) => cs.zipWithIndex.map{
      case (cell, colNum) => cell.nextState(this.countLiveNeighbors(rowNum, colNum))
    }
  })

  override def cellAt(row: Int, column: Int): Cell = grid.lift(row).flatMap(_.lift(column)).getOrElse(Dead)

  override def toString: String = {
    grid.map(_.map{
      case Alive => 'x'
      case Dead => '_'
    }.mkString("")).mkString("\n")
  }

  def countLiveNeighbors(row: Int, column: Int): Int = {
    Seq(
      cellAt(row - 1, column),
      cellAt(row + 1, column),
      cellAt(row, column - 1),
      cellAt(row, column + 1),
      cellAt(row - 1, column - 1),
      cellAt(row + 1, column - 1),
      cellAt(row + 1, column + 1),
      cellAt(row - 1, column + 1)
    ).collect{
      case Alive => 1
    }.sum
  }
}

object Grid {
  def main(args: Array[String]): Unit = {
    var g: GameOfLife = new Grid(Seq(
      Seq(Dead, Dead, Dead),
      Seq(Alive, Alive, Alive),
      Seq(Dead, Dead, Dead)
    ))

    while(true) {
      println("*********************")
      println(g)
      g = g.iterate();
    }
  }
}
