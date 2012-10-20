package pivot 

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PivotSuite extends FunSuite {
  trait PivotTest {
    val numberRows = (List("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten") +: 
      Range(1, 10, 1).map(x => {Range(x*1, x*1+10, 1).map(_.toString).toList})).toList
    val p1 = Pivot(numberRows)
    
    val marriedP = Pivot(
      List(
        List("Name",    "Age",  "Married"),
        List("Bill",    "42",   "TRUE"   ),
        List("Heloise", "47",   "TRUE"   ),
        List("Thelma",  "34",   "FALSE"  ),
        List("Bridget", "47",   "TRUE"   ),
        List("Robert",  "42",   "FALSE"  ),
        List("Eddie",   "42",   "TRUE"   )

      )
    )
  }

  test("headers should have same size as rows") {
    new PivotTest {
      p1.body.foreach(b => {
        assert(p1.headers.size === b.size)}
      )
    }
  }

  test("calculated columns should be appended to the row set") {
    new PivotTest {
      def createSum(row: List[String]) = {
        row.map(_.toInt).foldRight(0)(_ + _).toString 
      }
      val p2 = p1.addCalcCol(createSum)("sum")
    }
  }

  test("pivot should transpose") {
    new PivotTest {
      def count(values: List[String]) = {
        values.map(x => {1}).sum.toString
      }
      def sum(values: List[String]) = {
        values.map(_.toInt).sum.toString
      }
      def avg(values: List[String]) = {
        (values.map(_.toInt).sum / values.size).toString
      }
      println("base table: \n" + marriedP + "\n")

      println("using count:  \n" + marriedP.doPivot(count)("Age", "Married", "Age") + "\n")
      println("using sum:    \n" + marriedP.doPivot(sum  )("Age", "Married", "Age") + "\n")
      println("using average:\n" + marriedP.doPivot(avg  )("Age", "Married", "Age") + "\n")
    }
  }

}
