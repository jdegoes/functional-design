package net.degoes
/*
 * INTRODUCTION
 *
 * In Functional Design, immutable values often model solutions to a problem,
 * and they are transformed and combined using operators.
 *
 * Operators come in two primary flavors: unary operators, which are the ones
 * that transform values into other values with desired properties; and
 * binary operators, which combine two values into a single value.
 *
 * Composable operators accept and return similar types, which allows them
 * to be used repeatedly. For example, the `+` binary operator for integers
 * allows repeatedly adding numbers together (`1 + 2 + 3 + 4`) because the
 * return value of the operator is compatible with its input type.
 *
 * Composable operators allow you to generate a large variety of values out of
 * a smaller number of values. When values model solutions, then operators
 * allow you to build an infinite number of solutions by simply composing and
 * transforming other solutions.
 *
 * In this section, you'll see examples of composable operators on a variety
 * of immutable values, each of which models a solution to some problem.
 */

/**
 * FILE I/O - EXERCISE SET 1
 *
 * Consider an ETL application that loads a lot of data from files and FTP
 * servers using Java's InputStream.
 */
object input_stream {
  import java.io.InputStream

  final case class IStream(createInputStream: () => InputStream) { self =>

    /**
     * EXERCISE 1
     *
     * Create an operator `++` that returns a new `IStream`, which will read
     * all data from the first input stream, and then when that one is
     * exhausted, it will close the first input stream, make the second
     * input stream, and continue reading from the second one.
     */
    def ++(that: IStream): IStream = ???

    /**
     * EXERCISE 2
     *
     * Create an operator `orElse` that returns a new `IStream`, which will
     * try to create the first input stream, but if that fails by throwing
     * an exception, it will then try to create the second input stream.
     */
    def orElse(that: IStream): IStream = ???

    /**
     * EXERCISE 3
     *
     * Create an operator `buffered` that returns a new `IStream`, which will
     * create the input stream, but wrap it in Java's `BufferedInputStream`
     * before returning it.
     */
    def buffered: IStream = ???
  }
}

/**
 * EMAIL CLIENT - EXERCISE SET 2
 *
 * Consider a web email interface, which allows users to filter emails and
 * direct them to specific folders based on custom criteria.
 */
object email_filter {
  final case class Address(emailAddress: String)
  final case class Email(sender: Address, to: List[Address], subject: String, body: String)

  final case class EmailFilter(matches: Email => Boolean) { self =>

    /**
     * EXERCISE 1
     *
     * Add an "and" operator that will match an email if both the first and
     * the second email filter match the email.
     */
    def &&(that: EmailFilter): EmailFilter = ???

    /**
     * EXERCISE 2
     *
     * Add an "or" operator that will match an email if either the first or
     * the second email filter match the email.
     */
    def ||(that: EmailFilter): EmailFilter = ???

    /**
     * EXERCISE 3
     *
     * Add a "negate" operator that will match an email if this email filter
     * does NOT match an email.
     */
    def negate: EmailFilter = ???
  }
  object EmailFilter {
    def senderIs(address: Address): EmailFilter = EmailFilter(_.sender == address)

    def recipientIs(address: Address): EmailFilter = EmailFilter(_.to.contains(address))

    def subjectContains(phrase: String): EmailFilter = EmailFilter(_.subject.contains(phrase))

    def bodyContains(phrase: String): EmailFilter = EmailFilter(_.body.contains(phrase))
  }

  /**
   * EXERCISE 4
   *
   * Make an email filter that looks for subjects that contain the word
   * "discount", bodies that contain the word "N95", and which are NOT
   * addressed to "john@doe.com". Build this filter up compositionally
   * by using the defined constructors and operators.
   */
  lazy val emailFilter1 = ???
}

/**
 * DATA TRANSFORM - EXERCISE SET 3
 *
 * Consider an email marketing platform, which allows users to upload contacts.
 */
object contact_processing {
  final case class SchemaCSV(columnNames: List[String]) {
    def relocate(i: Int, j: Int): Option[SchemaCSV] =
      if (i < columnNames.length && j < columnNames.length)
        Some(copy(columnNames = columnNames.updated(i, columnNames(j)).updated(j, columnNames(i))))
      else None

    def delete(i: Int): SchemaCSV = copy(columnNames = columnNames.take(i) ++ columnNames.drop(i + 1))

    def add(name: String): SchemaCSV = copy(columnNames = columnNames ++ List(name))
  }

  final case class ContactsCSV(schema: SchemaCSV, content: Vector[Vector[String]]) { self =>
    def add(columnName: String, column: Vector[String]): ContactsCSV =
      copy(schema = schema.add(columnName), content = content :+ column)

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

    def rename(oldColumn: String, newColumn: String): ContactsCSV = {
      val index = schema.columnNames.indexOf(oldColumn)

      if (index < 0) self
      else copy(schema = SchemaCSV(schema.columnNames.updated(index, newColumn)))
    }

    def relocate(i: Int, j: Int): Option[ContactsCSV] =
      if (i < content.length && j < content.length)
        schema
          .relocate(i, j)
          .map(schema => copy(schema = schema, content = content.updated(j, content(i)).updated(i, content(j))))
      else None

    def delete(i: Int): ContactsCSV =
      copy(schema = schema.delete(i), content = content.take(i) ++ content.drop(i + 1))

    def combine(column1: String, column2: String)(
      newColumn: String
    )(f: (String, String) => String): Option[ContactsCSV] =
      for {
        _index1          <- columnOf(column1)
        _index2          <- columnOf(column2)
        (index1, index2) = if (_index1 < _index2) (_index1, _index2) else (_index2, _index1)
        column           = content(index1).zip(content(index2)).map(f.tupled)
      } yield add(newColumn, column).delete(index1).delete(index2)
  }

  sealed trait MappingResult[+A]
  object MappingResult {
    final case class Success[+A](warnings: List[String], value: A) extends MappingResult[A]
    final case class Failure(errors: List[String])                 extends MappingResult[Nothing]
  }

  final case class SchemaMapping(map: ContactsCSV => MappingResult[ContactsCSV]) { self =>

    /**
     * EXERCISE 1
     *
     * Add a `+` operator that combines two schema mappings into one, applying
     * the effects of both in sequential order. If the first schema mapping
     * fails, then the result must fail. If the second schema mapping fails,
     * then the result must also fail. Only if both schema mappings succeed
     * can the resulting schema mapping succeed.
     */
    def +(that: SchemaMapping): SchemaMapping = ???

    /**
     * EXERCISE 2
     *
     * Add an `orElse` operator that combines two schema mappings into one,
     * applying the effects of the first one, unless it fails, and in that
     * case, applying the effects of the second one.
     */
    def orElse(that: SchemaMapping): SchemaMapping = ???

    /**
     * EXERCISE 3
     *
     * Add an `exclude` operator that returns a new schema mapping that
     * excludes the specified column names from the schema mapping.
     */
    def exclude(columnNames: Set[String]): SchemaMapping = ???
  }
  object SchemaMapping {

    /**
     * EXERCISE 4
     *
     * Add a constructor for `SchemaMapping` that renames a column.
     */
    def rename(oldName: String, newName: String): SchemaMapping = ???

    /**
     * EXERCISE 5
     *
     * Add a constructor for `SchemaMapping` that combines two columns into one.
     */
    def combine(leftColumn: String, rightColumn: String)(
      newName: String,
      f: (String, String) => String
    ): SchemaMapping = ???

    /**
     * EXERCISE 5
     *
     * Add a constructor for `SchemaMapping` that moves the index of the ith
     * column to the jth position.
     */
    def relocate(i: Int, j: Int): SchemaMapping = ???

    /**
     * EXERCISE 6
     *
     * Add a constructor for `SchemaMapping` that deletes the column of the
     * specified name.
     */
    def delete(name: String): SchemaMapping = ???
  }

  /**
   * EXERCISE 7
   *
   * Create a schema mapping that can remap the user's uploaded schema into the
   * company's official schema for contacts, by composing schema mappings
   * constructed from constructors and operators.
   */
  lazy val schemaMapping: SchemaMapping = ???

  val UserUploadSchema: SchemaCSV =
    SchemaCSV(List("email", "fname", "lname", "country", "street", "postal"))

  val OfficialCompanySchema: SchemaCSV =
    SchemaCSV(List("full_name", "email_address", "country", "street_address", "postal_code"))
}

/**
 * CARD GAME - EXERCISE SET 4
 *
 * Consider a game such as FreeCell or Solitaire that is played using a deck of cards.
 */
object ui_events {
  sealed trait Suit
  object Suit {
    case object Clubs    extends Suit
    case object Diamonds extends Suit
    case object Hearts   extends Suit
    case object Spades   extends Suit
  }
  sealed trait Rank
  object Rank {
    case object Ace                   extends Rank
    case object King                  extends Rank
    case object Queen                 extends Rank
    case object Jack                  extends Rank
    final case class Numbered(n: Int) extends Rank
  }
  trait Card {
    def suit: Suit
    def rank: Rank
  }
  sealed trait GameEvent
  object GameEvent {
    final case class CardClick(card: Card) extends GameEvent
  }
  trait GameController {
    def addListener(listener: Listener): Unit
  }

  final case class Listener(onEvent: GameEvent => Unit) { self =>

    /**
     * EXERCISE 1
     *
     * Add a method `+` that composes two listeners into a single listener,
     * by sending each game event to both listeners.
     */
    def +(that: Listener): Listener = ???

    /**
     * EXERCISE 2
     *
     * Add a method `orElse` that composes two listeners into a single listener,
     * by sending each game event to either the left listener, if it does not
     * throw an exception, or the right listener, if the left throws an exception.
     */
    def orElse(that: Listener): Listener = ???

    /**
     * EXERCISE 3
     *
     * Add a `runOn` operator that returns a Listener that will call this one's
     * `onEvent` callback on the specified `ExecutionContext`.
     */
    def runOn(ec: scala.concurrent.ExecutionContext): Listener = ???

    /**
     * EXERCISE 4
     *
     * Add a `debug` unary operator that will call the `onEvent` callback, but
     * before it does, it will print out the game event to the console.
     */
    def debug: Listener = ???
  }
}

/**
 * EDUCATION - GRADUATION PROJECT
 *
 * Consider a console-based educational application that tests the user's
 * knowledge of key concepts.
 */
object education {
  // Here the type `A` represents the type of answer the user is expected to
  // fill in when answering the question.
  sealed trait Question[A] {
    def question: String
  }
  object Question {
    final case class Text(question: String)                                    extends Question[String]
    final case class MultipleChoice(question: String, choices: Vector[String]) extends Question[Int]
    final case class TrueFalse(question: String)                               extends Question[Boolean]
  }

  final case class QuizResult(correctPoints: Int, bonusPoints: Int, wrongPoints: Int, wrong: Vector[String]) {
    def totalPoints: Int = correctPoints + wrongPoints

    def toBonus: QuizResult = QuizResult(0, bonusPoints + correctPoints, 0, Vector.empty)

    /**
     * EXERCISE 1
     *
     * Add a `+` operator that combines this quiz result with the specified
     * quiz result.
     */
    def +(that: QuizResult): QuizResult = ???
  }
  object QuizResult {

    /**
     * EXERCISE 2
     *
     * Add an `empty` QuizResult that, when combined with any quiz result,
     * returns that same quiz result.
     */
    def empty: QuizResult = ???
  }

  final case class Quiz(run: () => QuizResult) { self =>

    /**
     * EXERCISE 3
     *
     * Add an operator `+` that appends this quiz to the specified quiz.
     */
    def +(that: Quiz): Quiz = ???

    /**
     * EXERCISE 4
     *
     * Add a unary operator `bonus` that marks this quiz as a bonus quiz.
     */
    def bonus: Quiz = ???

    /**
     * EXERCISE 5
     *
     * Add a conditional operator which, if the user gets this quiz right
     * enough, as determined by the specified cutoff, will do the `ifPass`
     * quiz afterward; but otherwise, do the `ifFail` quiz.
     */
    def conditional(cutoff: Int)(ifPass: Quiz, ifFail: Quiz): Quiz = ???
  }
  object Quiz {
    private def grade[A](answer0: => A, grader: Grader[A]): QuizResult =
      scala.util.Try {
        val answer = answer0

        grader.isCorrect(answer) match {
          case Left(string)  => QuizResult(0, 0, grader.points, Vector(string))
          case Right(string) => QuizResult(grader.points, 0, 0, Vector.empty)
        }
      }.getOrElse(QuizResult(0, 0, grader.points, Vector("The format of your answer was not recognized")))

    def single[A](question: Question[A], grader: Grader[A]): Quiz =
      Quiz { () =>
        import Question._

        println(question.question)
        val answer = scala.io.StdIn.readLine()

        question match {
          case Text(question)                    => grade(answer, grader)
          case MultipleChoice(question, choices) => grade(answer.toInt, grader)
          case TrueFalse(question)               => grade(answer.toLowerCase().startsWith("t"), grader)
        }
      }

    /**
     * EXERCISE 6
     *
     * Add an `empty` Quiz that does not ask any questions and only returns
     * an empty QuizResult.
     */
    def empty: Quiz = ???
  }

  final case class Grader[-A](points: Int, isCorrect: A => Either[String, Unit])
  object Grader {
    def isTrue(points: Int): Grader[Boolean] = Grader(points, if (_) Right(()) else Left("The correct answer is true"))
    def isFalse(points: Int): Grader[Boolean] =
      Grader(points, v => if (!v) Right(()) else Left("The correct answer is false"))
  }

  /**
   * EXERCISE 7
   *
   * Extend the following quiz with an additional 3 questions, including a
   * tough bonus question; and if the user fails the bonus question, fallback
   * to a simpler bonus question with fewer bonus points.
   */
  lazy val exampleQuiz: Quiz = ???
}
