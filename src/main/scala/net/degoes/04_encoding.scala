package net.degoes

/*
 * INTRODUCTION
 *
 * In Functional Design, there are two ways to encode a domain constructors and
 * operators:
 *
 * 1. As processes, typically using a function or an interface with methods in
 *    it that perform actual work. This is sometimes called the "final" encoding.
 *    It's a direct, "executable" encoding of a domain.
 *
 * 2. As a data structure, using sealed traits and case classes (ADTs). This is
 *    sometimes called the "initial" encoding in functional programming. It's an
 *    indirect, purely descriptive model of a domain.
 *
 * In the second encoding, a so-called "executor" or "interpreter" or "compiler"
 * translates the data structure, which describes a solution, into either
 * executable code or into another lower-level domain, which provides the
 * capabilities modeled by the domain.
 *
 * These two methods are equivalent; legacy code prefers (1), while often (2)
 * can be more useful, especially for new code.
 *
 */

/**
 * EDUCATION - EXERCISE SET 1
 *
 * Consider a console-based educational application that tests the user's
 * knowledge of key concepts.
 */
object education_final {
  final case class QuizResult(correctPoints: Int, bonusPoints: Int, wrongPoints: Int, wrong: Vector[String]) { self =>
    def totalPoints: Int = correctPoints + wrongPoints

    def toBonus: QuizResult = copy(correctPoints = 0, bonusPoints = bonusPoints + correctPoints)

    def +(that: QuizResult): QuizResult =
      QuizResult(
        self.correctPoints + that.correctPoints,
        self.bonusPoints + that.bonusPoints,
        self.wrongPoints + that.wrongPoints,
        self.wrong ++ that.wrong
      )
  }

  sealed trait Quiz {

    /**
     * EXERCISE 1
     *
     * Add an operator `+` that appends this quiz to the specified quiz. Model
     * this as pure data using a constructor for Quiz in the companion object.
     */
    def +(that: Quiz): Quiz = ???

    /**
     * EXERCISE 3
     *
     * Add a unary operator `bonus` that marks this quiz as a bonus quiz. Model
     * this as pure data using a constructor for Quiz in the companion object.
     */
    def bonus: Quiz = ???

    /**
     * EXERCISE 4
     *
     * Add a conditional operator which, if the user gets this quiz right,
     * will do the `ifPass` quiz afterward; but otherwise, do the `ifFail` quiz.
     * Model this as pure data using a constructor for Quiz in the companion
     * object.
     */
    def conditional(ifPass: Quiz, ifFail: Quiz): Quiz = ???
  }
  object Quiz {}

  /**
   * EXERCISE 5
   *
   * Implement an interpreter for the `Quiz` model that translates it into
   * the interactive console operations that it describes, returning a
   * QuizResult value.
   */
  def run(quiz: Quiz): QuizResult = ???
}

/**
 * DATA TRANSFORM - EXERCISE SET 2
 *
 * Consider an email marketing platform, which allows users to upload contacts.
 */
object contact_processing2 {
  sealed trait SchemaMapping {

    /**
     * EXERCISE 1
     *
     * Add a `+` operator that models combining two schema mappings into one,
     * applying the effects of both in sequential order.
     */
    def +(that: SchemaMapping): SchemaMapping = ???

    /**
     * EXERCISE 2
     *
     * Add an `orElse` operator that models combining two schema mappings into
     * one, applying the effects of the first one, unless it fails, and in that
     * case, applying the effects of the second one.
     */
    def orElse(that: SchemaMapping): SchemaMapping = ???

    /**
     * EXERCISE 3
     *
     * Add an `exclude` operator that models returning a schema that excludes
     * excludes the specified column names from this schema mapping.
     */
    def exclude(columnNames: Set[String]): SchemaMapping = ???
  }
  object SchemaMapping {

    /**
     * EXERCISE 4
     *
     * Add a constructor for `SchemaMapping` models renaming the column name.
     */
    def rename(oldName: String, newName: String): SchemaMapping = ???

    /**
     * EXERCISE 5
     *
     * Add a constructor for `SchemaMapping` that models moving the index of
     * the ith column to the jth position.
     */
    def relocate(i: Int, j: Int): SchemaMapping = ???

    /**
     * EXERCISE 6
     *
     * Add a constructor for `SchemaMapping` that models deleting the column
     * of the specified name.
     */
    def delete(name: String): SchemaMapping = ???
  }

  final case class SchemaCSV(columnNames: List[String])

  final case class ContactsCSV(schema: SchemaCSV, content: Vector[Vector[String]]) {
    def columnNames: List[String] = schema.columnNames

    def columnOf(name: String): Option[Int] = {
      val index = columnNames.indexOf(name)

      if (index >= 0) Some(index) else None
    }

    def get(row: Int, columnName: String): Option[String] =
      for {
        col   <- columnOf(columnName)
        row   <- content.lift(row)
        value <- row.lift(col)
      } yield value

  }

  val OurSchema: SchemaCSV =
    SchemaCSV(List("first_name", "last_name", "email_address", "country", "street_address", "postal_code"))

  sealed trait MappingResult[+A]
  object MappingResult {
    final case class Success[+A](warnings: List[String], value: A) extends MappingResult[A]
    final case class Failure(errors: List[String])                 extends MappingResult[Nothing]
  }

  /**
   * EXERCISE 7
   *
   * Implement an interpreter for the `SchemaMapping` model that translates it into
   * into changes on the contact list.
   */
  def run(mapping: SchemaMapping, contacts: ContactsCSV): MappingResult[ContactsCSV] = ???

  /**
   * EXERCISE 8
   *
   * Implement an optimizer for the `SchemaMapping` model that detects and eliminates
   * redundant renames; e.g. renaming "name" to "first_name", and then back to "name".
   */
  def optimize(schemaMapping: SchemaMapping): SchemaMapping = ???
}

/**
 * EMAIL CLIENT - EXERCISE SET 3
 *
 * Consider a web email interface, which allows users to filter emails and
 * direct them to specific folders based on custom criteria.
 */
object email_filter2 {
  final case class Address(emailAddress: String)
  final case class Email(sender: Address, to: List[Address], subject: String, body: String)

  sealed trait EmailFilter {

    /**
     * EXERCISE 1
     *
     * Add an "and" operator that models matching an email if both the first and
     * the second email filter match the email.
     */
    def &&(that: EmailFilter): EmailFilter = ???

    /**
     * EXERCISE 2
     *
     * Add an "or" operator that models matching an email if either the first or
     * the second email filter match the email.
     */
    def ||(that: EmailFilter): EmailFilter = ???

    /**
     * EXERCISE 3
     *
     * Add a "negate" operator that models matching an email if this email filter
     * does NOT match an email.
     */
    def negate: EmailFilter = ???
  }
  object EmailFilter {

    /**
     * EXERCISE 4
     *
     * Add a constructor for `EmailFilter` that models looking to see if the
     * subject of an email contains the specified word.
     */
    def subjectContains(string: String): EmailFilter = ???

    /**
     * EXERCISE 5
     *
     * Add a constructor for `EmailFilter` that models looking to see if the
     * body of an email contains the specified word.
     */
    def bodyContains(string: String): EmailFilter = ???

    /**
     * EXERCISE 6
     *
     * Add a constructor for `EmailFilter` that models looking to see if the
     * sender of an email is in the specified set of senders.
     */
    def senderIn(senders: Set[Address]): EmailFilter = ???

    /**
     * EXERCISE 7
     *
     * Add a constructor for `EmailFilter` that models looking to see if the
     * recipient of an email is in the specified set of recipients.
     */
    def recipientIn(recipients: Set[Address]): EmailFilter = ???
  }

  /**
   * EXERCISE 8
   *
   * Implement an interpreter for the `EmailFilter` model that translates it into
   * into tests on the specified email.
   */
  def matches(filter: EmailFilter, email: Email): Boolean = ???

  /**
   * EXERCISE 9
   *
   * Implement a function to print out an English-readable description of an
   * `EmailFilter`.
   */
  def describe(filter: EmailFilter): Unit = ???
}

/**
 * SPREADSHEET - EXERCISE SET 4
 *
 * Consider a spreadsheet application with a bunch of cells, containing either
 * static data or formula computed from other cells.
 */
object spreadsheet2 {
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
    sealed trait Static extends CellContents
    object Static {
      final case class Error(message: String) extends Static
      final case class Str(value: String)     extends Static
      final case class Dbl(value: Double)     extends Static
    }

    sealed trait Expr extends CellContents {

      /**
       * EXERCISE 1
       *
       * Add some operators to transform one `Expr` into another `Expr`. For
       * example, one operator could "negate" a double expression.
       */
      def negate: Expr = ???

      /**
       * EXERCISE 2
       *
       * Add some operators to combine `Expr`. For example, one operator
       * could sum two double expressions.
       */
      def sum(that: Expr): Expr = ???
    }
    object Expr {

      /**
       * EXERCISE 3
       *
       * Add some constructors for `Expr`, which can make `Expr` from values
       * outside the domain. For example, one constructor could make an `Expr`
       * from a `String`. Another constructor could sum a row/column range.
       */
      def fromString(s: String): Expr = ???
    }
  }

  /**
   * EXERCISE 4
   *
   * Implement an interpreter for the `CellContents.Expr` model that translates it into
   * static cell contents by evaluating the expression.
   */
  def evaluate(spreadsheet: Spreadsheet, cell: Cell): CellContents.Static = ???
}

/**
 * E-COMMERCE MARKETING - GRADUATION PROJECT
 *
 * Consider an e-commerce marketing platform where emails are sent to users
 * whose history matches specific patterns (for example, an event of adding
 * a product to a shopping card, followed by an abandonment of the web
 * session).
 */
object ecommerce_marketing {
  type Event = Map[Attribute, Value]

  sealed trait Attribute
  object Attribute {
    case object EventType      extends Attribute
    case object UserName       extends Attribute
    case object ShoppingCartId extends Attribute
    case object Email          extends Attribute
    case object WebSession     extends Attribute
    case object DateTime       extends Attribute
  }

  sealed trait Value
  object Value {
    final case class Str(value: String)                        extends Value
    final case class Id(value: String)                         extends Value
    final case class Email(value: String)                      extends Value
    final case class DateTime(value: java.time.OffsetDateTime) extends Value
  }

  object initial_encoding {
    sealed trait Pattern { self =>
      def +(that: Pattern): Pattern = Pattern.Sequence(self, that)

      def repeat(min: Option[Int], max: Option[Int]): Pattern = Pattern.Repeat(self, min, max)
    }
    object Pattern {
      final case class HasAttribute(attr: Attribute)                                extends Pattern
      final object HasAnyAttribute                                                  extends Pattern
      final case class HasValue(attr: Attribute, value: Value)                      extends Pattern
      final case class Sequence(first: Pattern, second: Pattern)                    extends Pattern
      final case class Repeat(pattern: Pattern, min: Option[Int], max: Option[Int]) extends Pattern
    }
    import Pattern._

    def matches(history: List[Event], pattern: Pattern): Boolean = {
      def loop(history: List[Event], pattern: Pattern): (List[Event], Boolean) =
        (pattern, history.headOption) match {
          case (HasAttribute(attr), Some(event))    => (history.tail, event.contains(attr))
          case (HasAnyAttribute, Some(event))       => (history.tail, true)
          case (HasValue(attr, value), Some(event)) => (history.tail, event.get(attr).map(_ == value).getOrElse(false))
          case (Sequence(first, second), _) =>
            val (leftHistory, leftMatch) = loop(history, first)

            if (leftMatch) loop(leftHistory, second) else (leftHistory, leftMatch)
          case (Repeat(pattern, min0, max0), _) =>
            val min = min0.getOrElse(0)
            val max = max0.getOrElse(Int.MaxValue)

            val baseline = (0 to min).foldLeft((history, true)) {
              case ((history, false), _) => (history, false)
              case ((history, true), _)  => loop(history, pattern)
            }

            if (!baseline._2) baseline
            else {
              val after = (0 to (max - min)).foldLeft(baseline) {
                case ((history, false), _) => (history, false)
                case ((history, true), _)  => loop(history, pattern)
              }

              (after._1, true)
            }
          case _ => (history, false)
        }
      loop(history, pattern)._2
    }
  }

  /**
   * EXERCISE 1
   *
   * Develop a final encoding of the pattern matcher. Instead of having an ADT
   * to represent a pattern, and then interpreting that on a user history to see
   * if there is a match, you will represent a pattern as a function or an
   * interface that is capable of testing the user history for a match.
   */
  object final_encoding {
    type Pattern
  }
}
