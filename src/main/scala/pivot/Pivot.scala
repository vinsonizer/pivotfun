package pivot

class Pivot(val rows: List[List[String]]) {

  val headers = rows.head.zipWithIndex.toMap
  val body    = rows.tail

  /** Adds a new calculated column to the end of the column set
   * 
   * @param f the function to be applied on the row to get the new column value
   * @param newColName what to name the header of this new column
   * @return a new Pivot instance with the new column at the end of each row
   */
  def addCalcCol(f: List[String] => String)(newColName: String) : Pivot = {
    val newRows = (headers.map(_._1).toList :+ newColName) +:
      body.map(row => { row :+ f(row)})
    Pivot(newRows)
  }

  /** Creates a new pivot structure by finding correlated values  <CR>
   * and performing an operation on these values
   *
   * @param accuOp the accumulator function (e.g. sum, max, etc)
   * @param xCol the "x" axis column
   * @param yCol the "y" axis column
   * @param accuCol the column to collect and perform accuOp on
   * @return a new Pivot instance that has been transformed with the accuOp function
   */
  def doPivot(accuOp: List[String] => String)(xCol: String, yCol: String, accuCol: String) = {
    // create subset that correlates to x, y, accuCol
    val pData = subset(List(xCol, yCol, accuCol))

    // group by x and y, sending the resulting collection of
    // accumulated values to the accuOp function for post-processing
    val data = pData.body.groupBy(row => {
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


  /** Create a smaller set of rows based on the headers selected
   * 
   * @param rowsToKeep header names desired in the result
   * @return a new Pivot with the subset of rows
   */
  def subset(rowsToKeep: List[String]) : Pivot = {
    val colsIdx = headerIndexes(rowsToKeep)
    Pivot(rows.map(r => {colsIdx.map(x => {r(x)})}))
  }

  /** Get indexes for a set of header names, defaulting to 1 if not found
   * 
   * @param hNames header names to lookup
   * @return List of Integers that represent the index in the column set, 
   *    defaulting to 1 if not found
   */
  private def headerIndexes(hNames: List[String]) = hNames.map(headers.getOrElse(_, 1))

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
