package net.degoes

object schedule_declarative:
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

  enum Schedule:
    case Weeks(weeks: Seq[Int])
    case DaysOfTheWeek(daysOfTheWeek: Seq[DayOfWeek])
    case HoursOfTheDay(hours: Seq[Int])
    case MinutesOfTheHour(minutes: Seq[Int])
    case Union(left: Schedule, right: Schedule)
    case Intersection(left: Schedule, right: Schedule)
    case Negation(schedule: Schedule)

    def self = this

    def ||(that: Schedule): Schedule = Union(self, that)

    def &&(that: Schedule): Schedule = Intersection(self, that)

    def ^^(that: Schedule): Schedule = (self || that) && !(self && that)

    def unary_! : Schedule = Negation(self)
  end Schedule
  object Schedule:
    def weeks(weeks: Int*): Schedule = Schedule.Weeks(weeks)
    def daysOfTheWeek(daysOfTheWeek: DayOfWeek*): Schedule = Schedule.DaysOfTheWeek(daysOfTheWeek)
    def hoursOfTheDay(hours: Int*): Schedule = Schedule.HoursOfTheDay(hours)
    def minutesOfTheHour(minutes: Int*): Schedule = Schedule.MinutesOfTheHour(minutes)
  end Schedule

  extension (self: Schedule) def fetchNow(time: Time): Boolean = 
    self match
      case Schedule.Weeks(weeks) => weeks.contains(time.weekOfMonth)
      case Schedule.DaysOfTheWeek(daysOfTheWeek) => daysOfTheWeek.contains(time.dayOfWeek)
      case Schedule.HoursOfTheDay(hours) => hours.contains(time.hourOfDay)
      case Schedule.MinutesOfTheHour(minutes) => minutes.contains(time.minuteOfHour)
      case Schedule.Union(left, right) => left.fetchNow(time) || right.fetchNow(time)
      case Schedule.Intersection(left, right) => left.fetchNow(time) && right.fetchNow(time)
      case Schedule.Negation(schedule) => !schedule.fetchNow(time)

  extension (self: Schedule) def prettyPrint(): String =
    self match
      case Schedule.Weeks(weeks) => weeks.mkString("Schedule.weeks(", ", ", ")")
      case Schedule.DaysOfTheWeek(daysOfTheWeek) => daysOfTheWeek.mkString("Schedule.daysOfTheWeek(", ", ", ")")
      case Schedule.HoursOfTheDay(hours) => hours.mkString("Schedule.hoursOfTheDay(", ", ", ")")
      case Schedule.MinutesOfTheHour(minutes) => minutes.mkString("Schedule.minutesOfTheHour(", ", ", ")")
      case Schedule.Union(left, right) => s"(${left.prettyPrint()} || ${right.prettyPrint()})"
      case Schedule.Intersection(left, right) => s"(${left.prettyPrint()} && ${right.prettyPrint()})"
      case Schedule.Negation(schedule) => s"!${schedule.prettyPrint()}"

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

/*
 * INTRODUCTION
 *
 * In Functional Design, there are two ways to represent models:
 *
 * 1. Using a function or interface, whose methods execute the solution. This is
 *    called the "executable" encoding in this course. It's a direct, executable
 *    encoding of a domain. If some functional domain is modeled with a class
 *    or case class, or an open trait that is implemented by classes, then it's
 *    probably an executable encoding.
 *
 * 2. Using a pure data structure, which declaratively describes the solution, but
 *    which does not perform the solution. It's an abstract, "declarative"
 *    encoding of a domain. If some functional domain type is modeled with a
 *    enum, then it's probably a declarative encoding, where the subtypes
 *    of the enum model individual operations and constructors in the
 *    domain.
 *
 * In the second encoding, a so-called "executor" or "interpreter" or "compiler"
 * translates the data structure, which merely models a solution, into either
 * executable code or into another lower-level domain, which provides the
 * capabilities modeled by the functional domain.
 *
 * Executable encodings are "open" for new constructors and operators: anyone
 * can add new constructors and operators, without updating existing code. On
 * the other hand, executable encodings are not "introspectable": because they
 * are not data, but rather, opaque executable machinery, it is not possible to
 * add new ways to execute the models without rewriting all constructors and
 * operators.
 *
 * Declarative encodings are "closed" for new constructors and operators: no
 * one can add new constructors and operators, without updating existing code.
 * Yet, because they are pure data, it is easy to add new ways to execute the
 * models, for example, serializers, optimizers, converters, and so forth,
 * assuming their component parts have the same properties (not all
 * declarative encodings do; if you embed a function inside a declarative
 * encoding, it becomes opaque).
 *
 * Summarizing the difference between executable and declarative encodings:
 *
 *  - Executable encodings have unbounded constructors/operators, but a fixed
 *    number of ways to execute them.
 *  - Declarative encodings have fixed constructors/operators, but an unbounded
 *    number of ways to execute them.
 *
 * Note: Tagless-final is an executable encoding, but one where, by making the
 * "solutions" polymorphic, the choice of executor can be deferred arbitrarily.
 *
 * Legacy code prefers executable encodings; while many benefits of Functional
 * Design can be seen best using abstract encodings.
 *
 */

/** EDUCATION - EXERCISE SET 1
  *
  * Consider a console-based educational application that tests the user's knowledge of key
  * concepts.
  */
object education_executable:
  import education.*

  enum Quiz2:
    case Sequence(left: Quiz2, right: Quiz2) 
    case ToBonus(value: Quiz2)
    case Single(question: Question[?]) 

    def self = this

    /** EXERCISE 1
      *
      * Add an operator `+` that appends this quiz to the specified quiz. Model this as pure data
      * using a constructor for Quiz in the companion object.
      */
    def +(that: Quiz2): Quiz2 = Sequence(self, that)

    /** EXERCISE 2
      *
      * Add a unary operator `bonus` that marks this quiz as a bonus quiz. Model this as pure data
      * using a constructor for Quiz in the companion object.
      */
    def bonus: Quiz2 = ToBonus(self)
  object Quiz2:
    def apply[A](question: Question[A]): Quiz2 = Single(question)

  /** EXERCISE 3
    *
    * Implement an interpreter for the `Quiz` model that translates it into the interactive console
    * operations that it describes, returning a QuizResult value.
    */
  def run(quiz: Quiz2): QuizResult = 
    def toOldQuiz(quiz2: Quiz2): Quiz = 
      quiz2 match
        case Quiz2.Sequence(left, right) => toOldQuiz(left) + toOldQuiz(right)
        case Quiz2.ToBonus(value) => toOldQuiz(value).bonus
        case Quiz2.Single(question) => Quiz(question)

    toOldQuiz(quiz).run()
end education_executable

/** DATA TRANSFORM - EXERCISE SET 2
  *
  * Consider an email marketing platform, which allows users to upload contacts.
  */
object contact_processing2:
  import contact_processing.*

  enum SchemaMapping2:
    case Sequence(left: SchemaMapping2, right: SchemaMapping2)
    case Fallback(left: SchemaMapping2, right: SchemaMapping2)
    case Rename(oldName: String, newName: String)
    case Delete(name: String)

    def self = this 

    /** EXERCISE 1
      *
      * Add a `+` operator that models combining two schema mappings into one, applying the effects
      * of both in sequential order.
      */
    def +(that: SchemaMapping2): SchemaMapping2 = Sequence(self, that)

    /** EXERCISE 2
      *
      * Add an `orElse` operator that models combining two schema mappings into one, applying the
      * effects of the first one, unless it fails, and in that case, applying the effects of the
      * second one.
      */
    def orElse(that: SchemaMapping2): SchemaMapping2 = Fallback(self, that)
  object SchemaMapping2:

    /** EXERCISE 3
      *
      * Add a constructor for `SchemaMapping` models renaming the column name.
      */
    def rename(oldName: String, newName: String): SchemaMapping2 = SchemaMapping2.Rename(oldName, newName)

    /** EXERCISE 4
      *
      * Add a constructor for `SchemaMapping` that models deleting the column of the specified name.
      */
    def delete(name: String): SchemaMapping2 = SchemaMapping2.Delete(name)

  /** EXERCISE 5
    *
    * Implement an interpreter for the `SchemaMapping` model that translates it into into changes on
    * the contact list.
    */
  def run(mapping: SchemaMapping2, contacts: ContactsCSV): MappingResult[ContactsCSV] = 
    def toOldMapping(mapping2: SchemaMapping2): SchemaMapping = 
      mapping2 match
        case SchemaMapping2.Sequence(left, right) => toOldMapping(left) + toOldMapping(right)
        case SchemaMapping2.Fallback(left, right) => toOldMapping(left) `orElse` toOldMapping(right)
        case SchemaMapping2.Rename(oldName, newName) => SchemaMapping.rename(oldName, newName)
        case SchemaMapping2.Delete(name) => SchemaMapping.delete(name)

    toOldMapping(mapping).map(contacts)

  /** BONUS EXERCISE
    *
    * Implement an optimizer for the `SchemaMapping` model that pushes deletes to the front of the
    * schema mapping in cases where doing so wouldn't later the result.
    */
  def optimize(schemaMapping: SchemaMapping2): SchemaMapping2 =
    ???
end contact_processing2

/** EMAIL CLIENT - EXERCISE SET 3
  *
  * Consider a web email interface, which allows users to filter emails and direct them to specific
  * folders based on custom criteria.
  */
object email_filter2:
  final case class Address(emailAddress: String)
  final case class Email(sender: Address, to: List[Address], subject: String, body: String)

  enum EmailFilter:
    case And(left: EmailFilter, right: EmailFilter) extends EmailFilter
    case Or(left: EmailFilter, right: EmailFilter) extends EmailFilter
    case Not(emailFilter: EmailFilter) extends EmailFilter
    case SubjectContains(text: String) extends EmailFilter
    case BodyContains(text: String) extends EmailFilter
    case SenderIn(senders: Set[Address]) extends EmailFilter
    case RecipientIn(recipients: Set[Address]) extends EmailFilter

    def self = this

    /** EXERCISE 1
      *
      * Add an "and" operator that models matching an email if both the first and the second email
      * filter match the email.
      */
    def &&(that: EmailFilter): EmailFilter = And(self, that)

    /** EXERCISE 2
      *
      * Add an "or" operator that models matching an email if either the first or the second email
      * filter match the email.
      */
    def ||(that: EmailFilter): EmailFilter = Or(self, that)

    /** EXERCISE 3
      *
      * Add a "negate" operator that models matching an email if this email filter does NOT match an
      * email.
      */
    def negate: EmailFilter = Not(self)
  end EmailFilter
  object EmailFilter:

    /** EXERCISE 4
      *
      * Add a constructor for `EmailFilter` that models looking to see if the subject of an email
      * contains the specified word.
      */
    def subjectContains(string: String): EmailFilter = SubjectContains(string)

    /** EXERCISE 5
      *
      * Add a constructor for `EmailFilter` that models looking to see if the body of an email
      * contains the specified word.
      */
    def bodyContains(string: String): EmailFilter = BodyContains(string)

    /** EXERCISE 6
      *
      * Add a constructor for `EmailFilter` that models looking to see if the sender of an email is
      * in the specified set of senders.
      */
    def senderIn(senders: Set[Address]): EmailFilter = SenderIn(senders)

    /** EXERCISE 7
      *
      * Add a constructor for `EmailFilter` that models looking to see if the recipient of an email
      * is in the specified set of recipients.
      */
    def recipientIn(recipients: Set[Address]): EmailFilter = RecipientIn(recipients)
  end EmailFilter

  /** EXERCISE 8
    *
    * Implement an interpreter for the `EmailFilter` model that translates it into into tests on the
    * specified email.
    */
  def matches(filter: EmailFilter, email: Email): Boolean =
    filter match
      case EmailFilter.And(left, right) =>
        matches(left, email) && matches(right, email)
      case EmailFilter.Or(left, right) =>
        matches(left, email) || matches(right, email)
      case EmailFilter.Not(emailFilter) =>
        !matches(emailFilter, email)
      case EmailFilter.SubjectContains(string) =>
        email.subject.contains(string)
      case EmailFilter.BodyContains(string) => 
        email.body.contains(string)
      case EmailFilter.SenderIn(senders) => 
        senders.contains(email.sender)
      case EmailFilter.RecipientIn(recipients) => 
        email.to.exists(recipients.contains(_))

  /** EXERCISE 9
    *
    * Implement a function to make an English-readable description of an `EmailFilter`.
    */
  def describe(filter: EmailFilter): String = filter match
    case EmailFilter.And(left, right) => (s"${describe(left)} && ${describe(right)}")
    case EmailFilter.Or(left, right) => (s"${describe(left)} || ${describe(right)}")
    case EmailFilter.Not(filter) => (s"!${describe(filter)}")
    case EmailFilter.SubjectContains(string) => (s"SubjectContains($string)")
    case EmailFilter.SenderIn(senders) => (s"SenderIn($senders)")
    case EmailFilter.BodyContains(string) => (s"BodyContains($string)")
    case EmailFilter.RecipientIn(recipients) => (s"RecipientIn($recipients)")

  def describePrint(filter: EmailFilter): Unit = println(describe(filter))
end email_filter2

/** SPREADSHEET - EXERCISE SET 4
  *
  * Consider a spreadsheet application with a bunch of cells, containing either static data or
  * formula computed from other cells.
  */
object spreadsheet2:
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

  enum CalculatedValue:
    case Negate(value: CalculatedValue)
    case Add(left: CalculatedValue, right: CalculatedValue)
    case At(col: Int, row: Int)
    case Const(value: Value)

    def self = this

    /** EXERCISE 1
      *
      * Add some operators to transform one `CalculatedValue` into another `CalculatedValue`. For
      * example, one operator could "negate" a double CalculatedValue.
      */
    def negate: CalculatedValue = CalculatedValue.Negate(self)

    /** EXERCISE 2
      *
      * Add some operators to combine `CalculatedValue`. For example, one operator could sum two
      * double CalculatedValueessions.
      */
    def sum(that: CalculatedValue): CalculatedValue = CalculatedValue.Add(self, that)
  object CalculatedValue:

    /** EXERCISE 3
      *
      * Add a constructor that makes an CalculatedValue from a Value.
      */
    def const(contents: Value): CalculatedValue = CalculatedValue.Const(contents)

    /** EXERCISE 4
      *
      * Add a constructor that provides access to the value of the specified cell, identified by
      * col/row.
      */
    def at(col: Int, row: Int): CalculatedValue = CalculatedValue.At(col, row)

  /** EXERCISE 5
    *
    * Implement an interpreter for the `Value.CalculatedValue` model that translates it into static
    * cell contents by evaluating the CalculatedValue.
    */
  def evaluate(spreadsheet: Spreadsheet, cell: Cell): Value = 
    def eval(value: CalculatedValue): Value = 
      value match
        case CalculatedValue.Negate(value) => 
          eval(value) match
            case Value.Error(message) => Value.Error(message)
            case Value.Str(value) => Value.Error("Can only negate numbers")
            case Value.Dbl(value) => Value.Dbl(-value)
          
        case CalculatedValue.Add(left, right) =>
          (eval(left), eval(right)) match 
            case (Value.Error(message), _) => Value.Error(message)
            case (_, Value.Error(message)) => Value.Error(message)
            case (Value.Str(value), _) => Value.Error("Can only add numbers")
            case (_, Value.Str(value)) => Value.Error("Can only add numbers")
            case (Value.Dbl(left), Value.Dbl(right)) => Value.Dbl(left + right)

        case CalculatedValue.At(col, row) => 
          spreadsheet.valueAt(col, row) match
            case CalculatedValue.Const(value) => value
            case _ => Value.Error("Can only reference cells with constant values")
            
        case CalculatedValue.Const(value) => value

    eval(cell.contents)
      
end spreadsheet2

/** E-COMMERCE MARKETING - GRADUATION PROJECT
  *
  * Consider an e-commerce marketing platform where emails are sent to users whose history matches
  * specific patterns (for example, an event of adding a product to a shopping card, followed by an
  * abandonment of the web session).
  */
object ecommerce_marketing:
  type Event = Map[Attribute, Value]

  enum Attribute:
    case EventType
    case UserName
    case ShoppingCartId
    case Email
    case WebSession
    case DateTime

  enum Value:
    case Str(value: String)
    case Id(value: String)
    case Email(value: String)
    case DateTime(value: java.time.OffsetDateTime)

  object abstract_encoding:
    enum HistoryPattern:
      case Matches
      case EventP(eventPattern: EventPattern)
      case Sequence(first: HistoryPattern, second: HistoryPattern)
      case Repeat(pattern: HistoryPattern, min: Option[Int], max: Option[Int])

      def self = this

      def *>(that: HistoryPattern): HistoryPattern = HistoryPattern.Sequence(self, that)

      def atLeast(n: Int): HistoryPattern = repeat(Some(n), None)

      def atMost(n: Int): HistoryPattern = repeat(None, Some(n))

      def between(min: Int, max: Int): HistoryPattern = repeat(Some(min), Some(max))

      def repeat(min: Option[Int], max: Option[Int]): HistoryPattern =
        HistoryPattern.Repeat(self, min, max)
    object HistoryPattern:

      val matches: HistoryPattern = Matches

      def event(eventPattern: EventPattern): HistoryPattern = EventP(eventPattern)

      def eventType(eventType: String): HistoryPattern =
        event(EventPattern.HasValue(Attribute.EventType, Value.Str(eventType)))
    enum EventPattern:
      case Matches
      case HasValue(attr: Attribute, value: Value)

      def self = this

      import EventPattern.*

      def matches(event: Event): Boolean =
        self match
          case Matches               => true
          case HasValue(attr, value) => event.get(attr) == Some(value)

    import HistoryPattern.*
    import Attribute.EventType

    val example = eventType("add-item") *> eventType("abandon-cart")

    def matches(history: List[Event], pattern: HistoryPattern): Boolean =
      def loop(history: List[Event], pattern: HistoryPattern): (List[Event], Boolean) =
        (pattern, history.headOption) match
          case (EventP(eventPattern), Some(event)) => (history.tail, eventPattern.matches(event))
          case (EventP(_), None)                   => (history.tail, false)
          case (Sequence(first, second), _)        =>
            val (leftHistory, leftMatch) = loop(history, first)

            if leftMatch then loop(leftHistory, second) else (leftHistory, leftMatch)
          case (Repeat(pattern, min0, max0), _)    =>
            val min = min0.getOrElse(0)
            val max = max0.getOrElse(Int.MaxValue)

            val baseline = (0 to min).foldLeft((history, true)):
              case ((history, false), _) => (history, false)
              case ((history, true), _)  => loop(history, pattern)

            if !baseline._2 then baseline
            else
              val after = (0 to (max - min)).foldLeft(baseline):
                case ((history, false), _) => (history, false)
                case ((history, true), _)  => loop(history, pattern)

              (after._1, true)
          case _                                   => (history, false)
      loop(history, pattern)._2
    end matches
  end abstract_encoding

  /** EXERCISE 1
    *
    * Develop an executable encoding of the pattern matcher. Instead of having an ADT to represent a
    * pattern, and then interpreting that on a user history to see if there is a match, you will
    * represent a pattern as a function or an interface that is capable of testing the user history
    * for a match.
    */
  object executable_encoding:
    import abstract_encoding.EventPattern

    final case class HistoryPattern(loop: List[Event] => (List[Event], Boolean)):
      def self = this 

      def matches(history: List[Event]): Boolean = loop(history)._2

      def *>(that: HistoryPattern): HistoryPattern = 
        HistoryPattern: history => 
          val (leftHistory, leftMatch) = self.loop(history)

          if leftMatch then that.loop(leftHistory) else (leftHistory, false)

      def atLeast(n: Int): HistoryPattern = repeat(Some(n), None)

      def atMost(n: Int): HistoryPattern = repeat(None, Some(n))

      def between(min: Int, max: Int): HistoryPattern = repeat(Some(min), Some(max))

      def repeat(min0: Option[Int], max0: Option[Int]): HistoryPattern =
        HistoryPattern: history =>
          val min = min0.getOrElse(0)
          val max = max0.getOrElse(Int.MaxValue)

          val baseline = (0 to min).foldLeft((history, true)):
            case ((history, false), _) => (history, false)
            case ((history, true), _)  => self.loop(history)

          if !baseline._2 then baseline
          else
            val after = (0 to (max - min)).foldLeft(baseline):
              case ((history, false), _) => (history, false)
              case ((history, true), _)  => self.loop(history)

            (after._1, true)
    object HistoryPattern:
      val matches: HistoryPattern = 
        HistoryPattern(history => (history, true))

      def event(eventPattern: EventPattern): HistoryPattern = 
        HistoryPattern:
          case event :: tail => (tail, eventPattern.matches(event))
          case history => (history, false)

      def eventType(eventType: String): HistoryPattern =
        event(EventPattern.HasValue(Attribute.EventType, Value.Str(eventType)))
end ecommerce_marketing
