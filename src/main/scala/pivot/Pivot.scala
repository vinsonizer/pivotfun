package pivot

class Pivot(val rows: List[List[String]]) {

  val headers = rows.head.zipWithIndex.map(_.swap)
  val body    = rows.tail

  def addCalcCol(f: List[String] => String)(newColName: String) : Pivot = {
    val newRows = (headers.map(_._2) :+ newColName) +:
      body.map(row => { row :+ f(row)})
    Pivot(newRows)
  }

  def doPivot(accuOp: List[String] => String)(xCol: String, yCol: String, accuCol: String) = {
    def colMatch(row: List[String], col : String) : Boolean = row(0) == col
    // transpose to easily filter out noise columns
    val tRows = rows.transpose
    val pivotRows = (tRows.filter(colMatch(_, xCol)) ++
        tRows.filter(colMatch(_, yCol)) ++
        tRows.filter(colMatch(_, accuCol))).transpose
    
    // group by x and y, sending the resulting collection of
    // accumulated values to the accuOp function for post-processing
    val data = pivotRows.tail.groupBy(row => {
      (row(0), row(1))
    }).map(g => {
      (g._1, accuOp(g._2.map(_(2))))
    }).toMap

    // get distinct axis values
    val xAxis = data.map(g => {g._1._1}).toList.distinct
    val yAxis = data.map(g => {g._1._2}).toList.distinct

    // create result matrix
    val newRows = yAxis.map(y => {
      xAxis.map(x => {
        data.getOrElse((x,y), "")
      })
    })

   // collect it with axis labels for results
   Pivot(List((yCol + "/" + xCol) +: xAxis) :::
     newRows.zip(yAxis).map(x=> {x._2 +: x._1}))
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
