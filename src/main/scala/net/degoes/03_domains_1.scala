package net.degoes

/*
 * INTRODUCTION
 *
 * In Functional Design, a functional domain consists of three things:
 *
 * 1. A set of types that model a solution to a domain problem.
 *
 * 2. Constructors that allow constructing simple solutions.
 *
 * 3. Operators that solving more complex problems by transforming
 *    and combining solutions for subproblems.
 *
 * Functional domains allow modeling solutions to problems in a specific domain.
 * Done properly, a small set of primitives can be so powerful, they can be used
 * compositionally to describe all possible solutions in that domain.
 *
 * A functional domain can be regarded as a type of internal domain-specific
 * language (DSL), which is designed specifically for expressing compositional
 * solutions to some category of domain problems.
 *
 * ZIO is an example of a domain for input/output, whose effect type lets you
 * solve async/concurrent/resourceful problems, and whose operators let you
 * assemble large solutions from small solutions.
 *
 * In this section, you'll learn about designing domains using ADTS,
 * constructors, and composable operators.
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
     * Design a subtype of `CellContents` called `CalculatedValue`, which
     * represents a value that is dynamically computed from a spreadsheet.
     */
    final case class CalculatedValue() extends CellContents { self =>

      /**
       * EXERCISE 2
       *
       * Add some operators to transform one `CalculatedValue` into another `CalculatedValue`. For
       * example, one operator could "negate" a double expression.
       */
      def negate: CalculatedValue = ???

      /**
       * EXERCISE 3
       *
       * Add some operators to combine `CalculatedValue`. For example, one operator
       * could sum two double expressions.
       */
      def sum(that: CalculatedValue): CalculatedValue = ???
    }
    object CalculatedValue {

      /**
       * EXERCISE 4
       *
       * Add a constructor that makes an CalculatedValue from a CellContents.
       */
      def const(contents: CellContents): CalculatedValue = ???

      /**
       * EXERCISE 5
       *
       * Add a constructor that provides access to the value of the
       * specified cell, identified by col/row.
       */
      def at(col: Int, row: Int): CalculatedValue = ???
    }
  }

  /**
   * EXERCISE 6
   *
   * Describe a cell whose contents are the sum of other cells.
   */
  lazy val cell1: Cell = ???
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
   * Design a data type that models the type of primitives the ETL pipeline
   * has access to. This will include string, numeric, and date/time data.
   */
  type DataType

  /**
   * EXERCISE 3
   *
   * Design a data type that models a value. Every value should have a `DataType`
   * that identifies its type (string, numeric, or data/time).
   */
  sealed trait DataValue {
    def dataType: DataType
  }

  /**
   * `Pipeline` is a data type that models a transformation from an input data
   * set into an output data step, as a series of one or more individual
   * operations.
   *
   * NOTE: This data type will purely *describe* steps in a pipeline. It will
   * not actually perform these steps. Separately, you could implement a
   * function to execute a pipeline by performing the steps it models, but
   * this task is beyond the scope of these exercises.
   */
  sealed trait Pipeline { self =>

    /**
     * EXERCISE 4
     *
     * Add a `merge` operator that models the merge of the output of this
     * pipeline with the input of the specified pipeline.
     */
    def merge(that: Pipeline): Pipeline = ???

    /**
     * EXERCISE 5
     *
     * Add an `orElse` operator that models applying this pipeline, but if it
     * fails, switching over and trying another pipeline.
     */
    def orElse(that: Pipeline): Pipeline = ???

    /**
     * EXERCISE 6
     *
     * Add an operator to rename a column in a pipeline.
     */
    def rename(oldName: String, newName: String): Pipeline = ???

    /**
     * EXERCISE 7
     *
     * Add an operator to coerce a column into a specific type in a pipeline.
     */
    def coerce(column: String, newType: DataType): Pipeline = ???

    /**
     * EXERCISE 8
     *
     * Add an operator to delete a column in a pipeline.
     */
    def delete(column: String): Pipeline = ???

    /**
     * EXERCISE 9
     *
     * To replace nulls in the specified column with a specified value.
     */
    def replaceNulls(column: String, defaultValue: DataValue): Pipeline = ???
  }
  object Pipeline {

    /**
     * EXERCISE 10
     *
     * Add a constructor for `Pipeline` that models extraction of data from
     * the specified data repository.
     */
    def extract(repo: DataRepo): Pipeline = ???
  }

  /**
   * EXERCISE 10
   *
   * Create a pipeline that models extracts data from a URL, replacing all null
   * "age" columns with "0" as the default age, which renames a column "fname"
   * into a column "first_name", and which coerces the "age" column into an
   * integer type.
   */
  lazy val pipeline: Pipeline = ???
}

/**
 * ANALYTICS - EXERCISE SET 3
 *
 * Consider a domain where you are doing in-memory analytics on values of type
 * `Double`. Mainly, this involves adding, multiplying, and so forth, on
 * "pages" of columnar data. A page could contain a certain number of "rows"
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
  final case class ColumnarPage() { self =>

    /**
     * EXERCISE 2
     *
     * Add an extend operation that extends the length of the page to the
     * specified length, by using "wraparound" semantics.
     */
    def ensureLength(n: Int): ColumnarPage = ???

    /**
     * EXERCISE 3
     *
     * Add a `+` operation that adds one page with another page by aligning the
     * rows in the two column pages and performing the operation pairwise. If
     * one page is shorter than the other, then use "wraparound" semantics for
     * the smaller page.
     */
    def +(that: ColumnarPage): ColumnarPage = ???

    /**
     * EXERCISE 4
     *
     * Add a `*` operation that multiplies one page with another page by
     * aligning the rows in the two column pages and performing the operation
     * pairwise. If one page is shorter than the other, then use "wraparound"
     * semantics for the smaller page.
     */
    def *(that: ColumnarPage): ColumnarPage = ???

    /**
     * EXERCISE 5
     *
     * Add a `-` operation that subtracts one page from another page by aligning
     * the rows in the two column pages and performing the operation pairwise.
     * If one page is shorter than the other, then use "wraparound" semantics
     * for the smaller page.
     */
    def -(that: ColumnarPage): ColumnarPage = ???

    /** EXERCISE 6
     *
     * Add a `reduce` operation that reduces the entire page down to a page
     * with only a single entry by using the user-specified combining function.
     */
    def reduce(f: (Double, Double) => Double): ColumnarPage = ???
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
   *
   * NOTE: It is acceptable for this data type to purely describe a schedule.
   * Separately, you could implement a function to execute a schedule, but
   * this task is beyond the scope of these exercises.
   */
  sealed trait Schedule {
    /*
     * EXERCISE 2
     *
     * Create an operator for schedule that allows composing two schedules to
     * yield the union of those schedules. That is, the fetch will occur
     * only when either of the schedules would have performed a fetch.
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
