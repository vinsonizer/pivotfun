package pivot

class Pivot(val rows: List[List[String]]) {

  val headers = rows.head.zipWithIndex.toMap
  val body    = rows.tail

  def addCalcCol(f: List[String] => String)(newColName: String) : Pivot = {
    val newRows = (headers.map(_._1).toList :+ newColName) +:
      body.map(row => { row :+ f(row)})
    Pivot(newRows)
  }

  def doPivot(accuOp: List[String] => String)(xCol: String, yCol: String, accuCol: String) = {
    // create list of indexes that correlate to x, y, accuCol
    val colsIdx = List(xCol, yCol, accuCol).map(headers.getOrElse(_, 1))

    // group by x and y, sending the resulting collection of
    // accumulated values to the accuOp function for post-processing
    val data = body.groupBy(row => {
      (row(colsIdx(0)), row(colsIdx(1)))
    }).map(g => {
      (g._1, accuOp(g._2.map(_(colsIdx(2)))))
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
