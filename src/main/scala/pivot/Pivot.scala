package pivot

class Pivot(val rows: List[List[String]]) {

  val headers = rows.head.zipWithIndex.map(_.swap)
  val body    = rows.tail

  def addCalcCol(f: List[String] => String)(newColName: String) : Pivot = {
    val newRows = (headers.map(_._2) :+ newColName) +:
      body.map(row => { row :+ f(row)})
    Pivot(newRows)
     
  }

  def doPivot(accuOp: (String => String))(xCol: String, yCol: String) = {
    val pivotRows = subset(List(xCol, yCol)).rows.map(row => {(row(0), row(1))})
    val header = pivotRows.head
    val data = pivotRows.tail.groupBy(identity)
    println(data)
 //  val data = pivotRows.tail.groupBy(identity).map(g => {(g._1, g._2.map(accuOp))})
 //  val xAxis = data.map(g => {g._1._1})
 //  val yAxis = data.map(g => {g._1._2})
 //  Pivot(xAxis.map(x => {
 //    yAxis.map(y => {
 //      data.filter(g => {g._1 == (x,y)}).get
 //    })
 //  }))
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
