package pivot

class Pivot(val rows: List[List[String]]) {

  val headers = rows.head.zipWithIndex.map(_.swap)
  val body    = rows.tail

  def addCalcCol(f: List[String] => String)(newColName: String) : Pivot = {
    val newRows = (headers.map(_._2) :+ newColName) +:
      body.map(row => { row :+ f(row)})
    Pivot(newRows)
     
  }

  def doPivot : Pivot = {
    Pivot(rows.transpose)
  }

  def subset(rowsToKeep: List[String]) : Pivot = {
    Pivot(this.rows.transpose.filter(row => {rowsToKeep.contains(row(0))}).transpose)
  }

  override def toString() = {
    val maxWidth = rows.map(row => { row.map(_.length).max}).max + 2
    rows.map(row => {
      row.map(_.padTo(maxWidth, " ").mkString).mkString
    }).mkString("\n")

  }

}

object Pivot {

  def apply(rows: List[List[String]]) = new Pivot(rows)

}
