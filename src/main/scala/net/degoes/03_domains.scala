package net.degoes

/*
 * INTRODUCTION
 *
 * In the last section, you explored operators. In this section, you will have
 * a chance to flesh out the design of full functional domains, which include
 * not only operators, but also models and constructors.
 */

/** SPREADSHEET - EXERCISE SET 1
  *
  * Consider a spreadsheet application with a bunch of cells, containing either static data or
  * formula computed from other cells.
  */
object spreadsheet:
  trait Spreadsheet:
    def cols: Int
    def rows: Int

    def valueAt(col: Int, row: Int): CalculatedValue

    final def scan(range: Range): LazyList[Cell] =
      val minRow = range.minRow.getOrElse(0)
      val maxRow = range.maxRow.getOrElse(rows - 1)

      val minCol = range.minCol.getOrElse(0)
      val maxCol = range.maxCol.getOrElse(cols - 1)

      (for
        col <- (minCol to maxCol).to(LazyList)
        row <- (minRow to maxRow).to(LazyList)
      yield Cell(col, row, valueAt(col, row)))

  final case class Range(
    minRow: Option[Int],
    maxRow: Option[Int],
    minCol: Option[Int],
    maxCol: Option[Int]
  )
  object Range:
    def column(i: Int): Range = Range(None, None, Some(i), Some(i))

    def row(i: Int): Range = Range(Some(i), Some(i), None, None)

  final case class Cell(col: Int, row: Int, contents: CalculatedValue)

  enum Value:
    case Error(message: String)
    case Str(value: String)
    case Dbl(value: Double)

  trait CalculatedValue2:
    def eval(spreadsheet: Spreadsheet): Value

  /** EXERCISE 1
    *
    * Design a data type called `CalculatedValue`, which represents a `Value` that is dynamically
    * computed from a `Spreadsheet`.
    */
  final case class CalculatedValue(eval: Spreadsheet => Value):
    self =>

    /** EXERCISE 2
      *
      * Add an operator that returns a new `CalculatedValue` that is the negated version of this
      * one.
      */
    def unary_- : CalculatedValue = 
      CalculatedValue: spreadsheet => 
        self.eval(spreadsheet) match 
          case Value.Dbl(value) => Value.Dbl(-value)
          case Value.Error(message) => Value.Error(message)
          case _ => Value.Error("Cannot negate non-numeric value")

    /** EXERCISE 3
      *
      * Add a binary operator `+` that returns a new `CalculatedValue` that is the sum of the two
      * calculated values.
      */
    def +(that: CalculatedValue): CalculatedValue = 
      binaryOp(that)("Cannot add non-numeric values"):
        case (Value.Dbl(left), Value.Dbl(right)) => Value.Dbl(left + right)

    /** EXERCISE 4
      *
      * Add a binary operator `-` that returns a new `CalculatedValue` that is the difference of the
      * two calculated values.
      */
    def -(that: CalculatedValue): CalculatedValue = 
      binaryOp(that)("Cannot subtract non-numeric values"):
        case (Value.Dbl(left), Value.Dbl(right)) => Value.Dbl(left - right)

    def *(that: CalculatedValue): CalculatedValue = 
      binaryOp(that)("Cannot multiply non-numeric values"):
        case (Value.Dbl(left), Value.Dbl(right)) => Value.Dbl(left * right)

    def concat(that: CalculatedValue): CalculatedValue = 
      binaryOp(that)("Cannot concatenate non-string values"):
        case (Value.Str(left), Value.Str(right)) => Value.Str(left + right)

    protected def binaryOp(that: CalculatedValue)(error: String)(
      f: PartialFunction[(Value, Value), Value]
    ): CalculatedValue = 
      CalculatedValue: spreadsheet => 
        val left = self.eval(spreadsheet)
        val right = that.eval(spreadsheet)
        f.lift((left, right)).getOrElse(Value.Error(error))
  end CalculatedValue
  object CalculatedValue:

    /** EXERCISE 5
      *
      * Add a constructor that makes an `CalculatedValue` from a `Value`.
      */
    def const(contents: Value): CalculatedValue = CalculatedValue(_ => contents)

    /** EXERCISE 6
      *
      * Add a constructor that provides access to the value of the specified cell, identified by
      * col/row.
      */
    def at(col: Int, row: Int): CalculatedValue = 
      CalculatedValue: spreadsheet => 
        spreadsheet.valueAt(col, row).eval(spreadsheet)

  /** EXERCISE 7
    *
    * Describe a cell whose contents are the sum of the cells at (0, 0) and (1, 0).
    */
  lazy val cell1: Cell = 
    Cell(2, 0, CalculatedValue.at(0, 0) + CalculatedValue.at(1, 0))
end spreadsheet

/** ETL - EXERCISE SET 2
  *
  * Consider an application designed to extract, transform, and load data.
  */
object etl:
  import scala.util.*

  /** Represents a row of data.
    */
  final case class DataRow(row: Map[String, DataValue]):
    self =>
    def delete(name: String): DataRow = DataRow(row - name)

    def map(name: String)(f: PartialFunction[DataValue, DataValue]): DataRow =
      row.get(name).fold(self)(v => f.lift(v).fold(self)(v => DataRow(row.updated(name, v))))

    def rename(oldName: String, newName: String): DataRow =
      DataRow(row.get(oldName).fold(row)(value => (row - oldName).updated(newName, value)))

    def coerce(name: String, dtype: DataType): DataRow =
      row.get(name).fold(self)(v => v.coerce(dtype).fold(self)(v => DataRow(row + (name -> v))))

  /** Represents a stream of data.
    */
  final case class DataStream(foreach: (Try[DataRow] => Unit) => Unit):
    self =>
    def coerce(name: String, dtype: DataType): DataStream = self.map(_.coerce(name, dtype))

    def delete(name: String): DataStream = self.map(_.delete(name))

    def orElse(that: => DataStream): DataStream =
      DataStream { callback =>
        self.foreach:
          case Failure(exception) => that.foreach(callback)
          case x                  => callback(x)
      }

    def map(f: DataRow => DataRow): DataStream =
      DataStream(callback => self.foreach(a => callback(a.map(f))))

    def mapColumn(name: String)(f: PartialFunction[DataValue, DataValue]): DataStream =
      self.map(_.map(name)(f))

    def merge(that: => DataStream): DataStream =
      DataStream { callback =>
        self.foreach(callback)
        that.foreach(callback)
      }

    def rename(oldName: String, newName: String): DataStream =
      self.map(_.rename(oldName, newName))
  end DataStream

  /** EXERCISE 1
    *
    * Design a data type that models sources and sinks in an ETL pipeline. Assume your business
    * requires you to extract data from (and load data to) FTP sites, URLs, AWS S3 buckets, and
    * databases described by JDBC connection strings.
    *
    * Also mock out, but do not implement, a method on each repository type called `load`, which
    * returns a `DataStream`.
    */
  enum DataRepo:
    case FTP(server: String, port: Int, credentials: (String, String), format: FileFormat)
    case URL(url: java.net.URL, format: FileFormat)
    case S3(bucket: String, token: String, format: FileFormat)
    case JDBC(server: String, port: Int, properties: java.util.Properties)

    def load: DataStream = ???

  enum FileFormat:
    case Json
    case Csv
    case Xml

  /** EXERCISE 2
    *
    * Design a data type that models the type of primitives the ETL pipeline has access to. This
    * will include string, numeric, and date/time data.
    */
  enum DataType:
    case Null
    case Text 
    case Numeric
    case DateTime

  /** EXERCISE 3
    *
    * Design a data type that models a value. Every value should have a `DataType` that identifies
    * its type (string, numeric, or data/time), and a `coerce` method to coerce the value into
    * another type.
    *
    * Be sure to model null, string, and integer, at the very least!
    */
  enum DataValue:
    case Null
    case Text(value: String)
    case Numeric(value: BigDecimal)
    case DateTime(value: java.time.Instant)

    def self = this

    def dataType: DataType =
      self match
        case Null => DataType.Null
        case Text(_) => DataType.Text
        case Numeric(_) => DataType.Numeric
        case DateTime(_) => DataType.DateTime

    def coerce(otherType: DataType): Option[DataValue] =
      self match
        case Null =>
          otherType match
            case DataType.Null => Some(Null)
            case DataType.Text => Some(Text(""))
            case DataType.Numeric => Some(Numeric(BigDecimal(0)))
            case DataType.DateTime => Some(DateTime(java.time.Instant.EPOCH))

        case Text(value) =>
          otherType match 
            case DataType.Text => Some(Text(value))
            case DataType.Numeric => Try(BigDecimal(value)).toOption.map(Numeric(_))
            case DataType.DateTime => Try(java.time.Instant.parse(value)).toOption.map(DateTime(_))
            case DataType.Null => if value == "" then Some(Null) else None

        case Numeric(value) =>
          otherType match 
            case DataType.Text => Some(Text(value.toString))
            case DataType.Numeric => Some(Numeric(value))
            case DataType.DateTime => Try(java.time.Instant.ofEpochMilli(value.toLong)).toOption.map(DateTime(_))
            case DataType.Null => if value == BigDecimal(0) then Some(Null) else None

        case DateTime(value) => 
          otherType match 
            case DataType.Text => Some(Text(value.toString))
            case DataType.Numeric => Some(Numeric(BigDecimal(value.toEpochMilli)))
            case DataType.DateTime => Some(DateTime(value))
            case DataType.Null => if value == java.time.Instant.EPOCH then Some(Null) else None
        
  end DataValue

  /** EXERCISE 4
    *
    * `Pipeline` is a data type that models a transformation from an input data set into an output
    * data step, as a series of one or more individual operations.
    *
    * Create a model of a pipeline, using `DataStream`.
    */
  final case class Pipeline(extractTransform: () => DataStream):
    self =>

    /** EXERCISE 5
      *
      * Add a `merge` operator that models the merge of the output of this pipeline with the output
      * of the specified pipeline.
      *
      * {{{
      * Merge Associativity:  (p1 merge p2) merge p3 == p1 merge (p2 merge p3)
      * Merge Identity:       p merge Pipeline.empty == Pipeline.empty merge p == p
      * Merge Commutativity:  p1 merge p2 == p2 merge p1
      * Merge Duplication:    ???
      * }}}
      */
    def merge(that: Pipeline): Pipeline = 
      Pipeline(() => self.extractTransform().merge(that.extractTransform()))

    /** EXERCISE 6
      *
      * Add an `orElse` operator that models applying this pipeline, but if it fails, switching over
      * and trying another pipeline.
      */
    def orElse(that: => Pipeline): Pipeline = 
      Pipeline(() => self.extractTransform().orElse(that.extractTransform()))

    /** EXERCISE 7
      *
      * Add an operator to rename a column in a pipeline.
      */
    def rename(oldName: String, newName: String): Pipeline = 
      Pipeline(() => self.extractTransform().rename(oldName, newName))

    /** EXERCISE 8
      *
      * Add an operator to coerce a column into a specific type in a pipeline.
      */
    def coerce(column: String, newType: DataType): Pipeline = 
      Pipeline(() => self.extractTransform().coerce(column, newType))

    /** EXERCISE 9
      *
      * Add an operator to delete a column in a pipeline.
      */
    def delete(column: String): Pipeline =
      Pipeline(() => self.extractTransform().delete(column))

    /** EXERCISE 10
      *
      * To replace nulls in the specified column with a specified value.
      */
    def replaceNulls(column: String, defaultValue: DataValue): Pipeline = 
      Pipeline(() => self.extractTransform().mapColumn(column) {
        case DataValue.Null => defaultValue
      })
  end Pipeline
  object Pipeline:

    /** EXERCISE 11
      *
      * Add a constructor for `Pipeline` that models extraction of data from the specified data
      * repository.
      */
    def extract(repo: DataRepo): Pipeline = 
      Pipeline(() => repo.load)

  /** EXERCISE 12
    *
    * Create a pipeline that models extracts data from a URL, replacing all null "age" columns with
    * "0" as the default age, which renames a column "fname" into a column "first_name", and which
    * coerces the "age" column into an integer type.
    */
  lazy val pipeline: Pipeline = 
    Pipeline.extract(DataRepo.URL(new java.net.URL("http://mywebsite.com/data.csv"), FileFormat.Csv))
      .replaceNulls("age", DataValue.Numeric(0))
      .rename("fname", "first_name")
      .coerce("age", DataType.Numeric)
end etl

/** REAL ESTATE APP - GRADUATION PROJECT
  *
  * Consider a real estate app that must regularly fetch third-party pricing data according to
  * specified schedules. These schedules can be quite complicated, although they possess regular
  * structure (e.g. every fifth Tuesday, and hourly on Wednesdays). The business considers it
  * acceptable to create the schedules in code (rather than reading them from a database).
  */
object pricing_fetcher:
  def fetch(directory: java.io.File, url: java.net.URL, schedule: Schedule): Unit = ???

  enum DayOfWeek:
    case Sunday
    case Monday
    case Tuesday
    case Wednesday
    case Thursday
    case Friday
    case Saturday

  final case class Time(
    minuteOfHour: Int,
    hourOfDay: Int,
    dayOfWeek: DayOfWeek,
    weekOfMonth: Int,
    monthOfYear: Int
  )

  /**
    * A Java-style interface that gives us the ability to model schedules.
    */
  // trait Schedule {
  //   def fetchNow(time: Time): Boolean
  //   def prettyPrint(): String
  // }

  /** EXERCISE 1
    *
    * `Schedule` is a data type that models a schedule, which has the ability to indicate whether at
    * any given `java.time.Instant`, it is time to fetch the pricing data set.
    */
  final case class Schedule(fetchNow: Time => Boolean, prettyPrint: () => String):
    self =>
    /*
     * EXERCISE 2
     *
     * Create an operator for schedule that allows composing two schedules to
     * yield the union of those schedules. That is, the fetch will occur
     * only when either of the schedules would have performed a fetch.
     */
    def ||(that: Schedule): Schedule = 
      Schedule(
        time => self.fetchNow(time) || that.fetchNow(time),
        () => s"(${self.prettyPrint()} || ${that.prettyPrint()})"
      )

    /** EXERCISE 3
      *
      * Create an operator for schedule that allows composing two schedules to yield the
      * intersection of those schedules. That is, the fetch will occur only when both of the
      * schedules would have performed a fetch.
      */
    def &&(that: Schedule): Schedule = 
      Schedule(
        time => self.fetchNow(time) && that.fetchNow(time),
        () => s"(${self.prettyPrint()} && ${that.prettyPrint()})"
      )

    def ^^(that: Schedule): Schedule = (self || that) && !(self && that)

    /** EXERCISE 4
      *
      * Create a unary operator that returns a schedule that will never fetch when the original
      * schedule would fetch, and will always fetch when the original schedule would not fetch.
      */
    def unary_! : Schedule = 
      Schedule(
        time => !self.fetchNow(time),
        () => s"!(${self.prettyPrint()})")
  end Schedule
  object Schedule:

    /** EXERCISE 5
      *
      * Create a constructor for Schedule that models fetching on specific weeks of the month.
      */
    def weeks(weeks: Int*): Schedule =
      val set = weeks.toSet

      Schedule(
        time => set.contains(time.weekOfMonth),
        () => s"Schedule.weeks(${weeks.mkString(", ")})"
      )

    /** EXERCISE 6
      *
      * Create a constructor for Schedule that models fetching on specific days of the week.
      */
    def daysOfTheWeek(daysOfTheWeek: DayOfWeek*): Schedule = 
      val set = daysOfTheWeek.toSet

      Schedule(
        time => set.contains(time.dayOfWeek),
        () => s"Schedule.daysOfTheWeek(${daysOfTheWeek.mkString(", ")})"
      )

    /** EXERCISE 7
      *
      * Create a constructor for Schedule that models fetching on specific hours of the day.
      */
    def hoursOfTheDay(hours: Int*): Schedule = 
      val set = hours.toSet

      Schedule(
        time => set.contains(time.hourOfDay), 
        () => s"Schedule.hoursOfTheDay(${hours.mkString(", ")})"
      )

    /** EXERCISE 8
      *
      * Create a constructor for Schedule that models fetching on specific minutes of the hour.
      */
    def minutesOfTheHour(minutes: Int*): Schedule = 
      val set = minutes.toSet

      Schedule(
        time => set.contains(time.minuteOfHour),
        () => s"Schedule.minutesOfTheHour(${minutes.mkString(", ")})"
      )
  end Schedule

  extension (s: Schedule) def logged: Schedule = 
    Schedule(
      time => 
        val result = s.fetchNow(time)
        println(s"Schedule ${if result then "fetched" else "did not fetch"} at ${time}")
        result,
      () => s"${s.prettyPrint()}.logged")

  /** EXERCISE 9
    *
    * Create a schedule that repeats every Wednesday, at 6:00 AM and 12:00 PM, and at 5:30, 6:30,
    * and 7:30 every Thursday.
    */
  lazy val schedule: Schedule = 
    (everyWednesday && (at6am || at12pm)) ||
    (everyThursday && (at5_30 || at6_30 || at7_30))

  val at6am  = Schedule.hoursOfTheDay(6)
  val at12pm = Schedule.hoursOfTheDay(12)

  val at5_30 = Schedule.hoursOfTheDay(5) && Schedule.minutesOfTheHour(30)
  val at6_30 = Schedule.hoursOfTheDay(6) && Schedule.minutesOfTheHour(30)
  val at7_30 = Schedule.hoursOfTheDay(7) && Schedule.minutesOfTheHour(30)

  val everyWednesday = Schedule.daysOfTheWeek(DayOfWeek.Wednesday)
  val everyThursday  = Schedule.daysOfTheWeek(DayOfWeek.Thursday)

  @main
  def test2 = println(schedule.logged.prettyPrint())
end pricing_fetcher
