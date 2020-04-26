package net.degoes

/*
 * INTRODUCTION
 *
 * In Functional Design, a domain consists of three things:
 *
 * 1. A set of types that describe a solution to a domain problem.
 *
 * 2. Constructors that allow constructing elements in the domain.
 *
 * 3. Operators that allow transforming & combining domain elements.
 *
 * ZIO is an example of a domain for input/output, whose effect type lets you
 * solve async/concurrent/resourceful problems, and whose operators let you
 * assemble large solutions from small solutions.
 *
 * In this section, you'll learn about designing domains.
 */

/**
 * SPREADSHEET - EXERCISE SET 1
 *
 * Consider a spreadsheet application with a bunch of cells, containing either
 * static data or formula computed from other cells.
 */
object spreadsheet {
  trait Spreadsheet {
    def cols: Int
    def rows: Int

    def valueAt(col: Int, row: Int): CellContents

    final def scan(range: Range): Stream[Cell] = {
      val minRow = range.minRow.getOrElse(0)
      val maxRow = range.maxRow.getOrElse(rows - 1)

      val minCol = range.minCol.getOrElse(0)
      val maxCol = range.maxCol.getOrElse(cols - 1)

      (for {
        col <- (minCol to maxCol).toStream
        row <- (minRow to maxRow).toStream
      } yield Cell(col, row, valueAt(col, row)))
    }
  }

  final case class Range(minRow: Option[Int], maxRow: Option[Int], minCol: Option[Int], maxCol: Option[Int])
  object Range {
    def column(i: Int): Range = Range(None, None, Some(i), Some(i))

    def row(i: Int): Range = Range(Some(i), Some(i), None, None)
  }

  final case class Cell(col: Int, row: Int, contents: CellContents)

  sealed trait CellContents
  object CellContents {
    final case class Error(message: String) extends CellContents
    final case class Str(value: String)     extends CellContents
    final case class Dbl(value: Double)     extends CellContents

    /**
     * EXERCISE 1
     *
     * Design a subtype of `CellContents` called `Expr`, which represents an
     * expression dynamically computed from the spreadsheet. Make sure that
     * `Expr` is a final case class.
     */
    final case class Expr() extends CellContents {

      /**
       * EXERCISE 2
       *
       * Add some operators to transform one `Expr` into another `Expr`. For
       * example, one operator could "negate" a double expression.
       */
      def negate: Expr = ???

      /**
       * EXERCISE 3
       *
       * Add some operators to combine `Expr`. For example, one operator
       * could sum two double expressions.
       */
      def sum(that: Expr): Expr = ???
    }
    object Expr {

      /**
       * EXERCISE 4
       *
       * Add some constructors for `Expr`, which can make `Expr` from values
       * outside the domain. For example, one constructor could make an `Expr`
       * from a `String`. Another constructor could sum a row/column range.
       */
      def fromString(s: String): Expr = ???
    }
  }

}

/**
 * ETL - EXERCISE SET 2
 *
 * Consider an application designed to extract, transform, and load data.
 *
 * NOTE: In these exercises, you will only create a data model to describe
 * ETL workflows. You will not actually implement any ETL. Such a data
 * model would need to be "executed" in order to be useful.
 */
object etl {

  /**
   * EXERCISE 1
   *
   * Design a data type that models sources and sinks in an ETL pipeline. Assume
   * your business requires you to extract data from (and load data to) FTP sites,
   * URLs, AWS S3 buckets, and databases described by JDBC connection strings.
   */
  type DataRepo

  /**
   * EXERCISE 2
   *
   * Design a data type that models the type of data the ETL pipeline has access
   * to. This will include string, numeric, and date/time data.
   */
  type DataType

  /**
   * EXERCISE 3
   *
   * Design a data type that models a value. Every value should have a `DataType`
   * that identifies its type (string, numeric, or data/time).
   */
  trait DataValue {
    def dataType: DataType
  }

  /**
   * EXERCISE 3
   *
   * Design a data type that models a step in an ETL pipeline. A step could be
   * one of the following:
   *
   * * Extracting data from a DataSource.
   * * Renaming a column in a pipeline
   * * Coercing a column into a specific type in a pipeline
   * * Replacing nulls with a given value in a pipeline
   * * Loading data into
   */
  trait Pipeline {

    /** EXERCISE 4
     *
     * Add an operator to rename a column in a pipeline.
     */
    def rename(oldName: String, newName: String): Pipeline

    /**
     * EXERCISE 5
     *
     * Add an operator to coerce a column into a specific type in a pipeline.
     */
    def coerce(column: String, newType: DataType): Pipeline

    /**
     * EXERCISE 6
     *
     * Add an operator to delete a column in a pipeline.
     */
    def delete(column: String): Pipeline

    /**
     * EXERCISE 7
     *
     * To replace nulls in the specified column with a specified value.
     */
    def replaceNulls(column: String, value: DataValue): Pipeline
  }
}

/**
 * ANALYTICS - EXERCISE SET 3
 *
 * Consider a domain where you are doing in-memory analytics on values of type
 * `Double`. Mainly, this involves adding, multiplying, and so forth, on
 * "pages"" of columnar data. A page could contain a certain number of "rows"
 * of Double data.
 */
object analytics {

  /**
   * EXERCISE 1
   *
   * Design a data type that holds a "page" worth of columnar `Double` data.
   * For efficiency, the page should be stored inside an `Array[Double]`, but
   * not exposed outside the data type.
   */
  trait ColumnarPage {

    /** EXERCISE 2
     *
     * Add a `+` operation that adds one page with another page by aligning the
     * rows in the two column pages and performing the operation pairwise. If
     * one page is shorter than the other, then use "wraparound" semantics for
     * the smaller page.
     */
    def +(that: ColumnarPage): ColumnarPage = ???

    /**
     * EXERCISE 3
     *
     * Add a `*` operation that multiplies one page with another page by
     * aligning the rows in the two column pages and performing the operation
     * pairwise. If one page is shorter than the other, then use "wraparound"
     * semantics for the smaller page.
     */
    def *(that: ColumnarPage): ColumnarPage = ???

    /**
     * EXERCISE 4
     *
     * Add a `-` operation that subtracts one page from another page by aligning
     * the rows in the two column pages and performing the operation pairwise.
     * If one page is shorter than the other, then use "wraparound" semantics
     * for the smaller page.
     */
    def -(that: ColumnarPage): ColumnarPage = ???

    /** EXERCISE 5
     *
     * Add a `reduce` operation that reduces the entire page down to a page
     * with only a single entry by using the user-specified combining function.
     */
    def reduce(f: (Double, Double) => Double): ColumnarPage = ???

    /**
     * EXERCISE 6
     *
     * Add an `extend(n: Int)` operation that extends the length of the page
     * to the specified number, by using "wraparound" semantics.
     */
    def extend(n: Int): Int
  }
  object ColumnarPage {

    /**
     * EXERCISE 6
     *
     * Add a constructor for ColumnarPage that converts the specified values
     * into a page.
     */
    def apply(doubles: Double*): ColumnarPage = ???
  }
}

/**
 * REAL ESTATE APP - GRADUATION PROJECT
 *
 * Consider a real estate app that must regularly fetch third-party pricing data
 * according to specified schedules. These schedules can be quite complicated,
 * although they possess regular structure (e.g. every fifth Tuesday, and hourly
 * on Wednesdays). The business considers it acceptable to create the schedules in
 * code (rather than reading them from a database).
 */
object pricing_fetcher {
  def fetch(directory: java.io.File, url: java.net.URL, schedule: Schedule): Unit = ???

  sealed trait DayOfWeek
  object DayOfWeek {
    case object Sunday    extends DayOfWeek
    case object Monday    extends DayOfWeek
    case object Tuesday   extends DayOfWeek
    case object Wednesday extends DayOfWeek
    case object Thursday  extends DayOfWeek
    case object Friday    extends DayOfWeek
    case object Saturday  extends DayOfWeek
  }

  /**
   * EXERCISE 1
   *
   * Create a data type `Schedule` that models a schedule.
   */
  trait Schedule {
    /*
     * EXERCISE 2
     *
     * Create an operator for schedule that allows composing two schedules to
     * yield the union of those schedules. That is, the fetch will occur
     * only when either of the schedules would have performed a fetch.
     *
     * NOTE: It is acceptable to only model the solution with a data constructor.
     */
    def union(that: Schedule): Schedule = ???

    /**
     * EXERCISE 3
     *
     * Create an operator for schedule that allows repeating one schedule inside
     * another schedule. For example, `first.interleave(second)` would at every fetch of
     * of the first schedule, switch over and fetch according to the second schedule,
     * and then resume according to the first schedule.
     *
     * NOTE: It is acceptable to only model the solution with a data constructor.
     */
    def interleave(that: Schedule): Schedule = ???

    /**
     * EXERCISE 4
     *
     * Create a unary operator that models a schedule repeat only a fixed number
     * of times, e.g. 10 times.
     *
     * NOTE: It is acceptable to only model the solution with a data constructor.
     */
    def times(times: Int): Schedule = ???
  }
  object Schedule {

    /**
     * EXERCISE 5
     *
     * Create a constructor for Schedule that models fetching on specific weeks
     * of the month.
     */
    def weeks(weeks: Int*): Schedule = ???

    /**
     * EXERCISE 6
     *
     * Create a constructor for Schedule that models fetching on specific days
     * of the week.
     */
    def daysOfTheWeek(daysOfTheWeek: Int*): Schedule = ???

    /** EXERCISE 7
     *
     * Create a constructor for Schedule that models fetching on specific hours.
     */
    def hours(hours: Int*): Schedule = ???

    /**
     * EXERCISE 8
     *
     * Create a constructor for Schedule that models fetching on specific minutes
     * of the hour.
     */
    def minutes(minutes: Int*): Schedule = ???
  }

  /**
   * EXERCISE 9
   *
   * Create a schedule that repeats every Wednesday, at 6:00 AM and 12:00 PM,
   * and at 5:30, 6:30, and 7:30 every Thursday.
   */
  lazy val schedule: Schedule = ???
}
