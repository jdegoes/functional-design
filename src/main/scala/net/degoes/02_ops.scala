package net.degoes
/*
 * INTRODUCTION
 *
 * In Functional Design, immutable values often model solutions to a problem,
 * and they are transformed and composed using operators.
 *
 * Operators come in two primary flavors: unary operators, which are the ones
 * that transform solutions into solutions values with desired properties; and
 * binary operators, which combine two solutions for subproblems into a
 * solution for a larger problem.
 *
 * Composable operators accept and return similar types, which allows them
 * to be used repeatedly. For example, the `+` binary operator for integers
 * allows repeatedly adding numbers together (`1 + 2 + 3 + 4`) because the
 * return value of the operator is compatible with its input type.
 *
 * Composable operators allow you to generate a large variety of solutions out
 * of a smaller number of primitives, simply transforming and composing other
 * solutions.
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

  /**
   * EXERCISE 4
   *
   * Construct an IStream that will read from `primary`,
   * or will read from the concatenation of all `secondaries`,
   * and will buffer everything.
   */
  lazy val solution: IStream = ???

  lazy val primary: IStream           = ???
  lazy val secondaries: List[IStream] = ???
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
    def get(column: String): Option[Vector[String]] =
      columnOf(column).map(i => content.map(row => row(i)))

    def add(columnName: String, column: Vector[String]): ContactsCSV =
      copy(schema = schema.add(columnName), content = content.zip(column).map { case (xs, x) => xs :+ x })

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

    def relocate(column: String, j: Int): Option[ContactsCSV] =
      columnOf(column).flatMap { i =>
        if (i < columnNames.length && j < columnNames.length)
          schema
            .relocate(i, j)
            .map(schema =>
              copy(schema = schema, content = content.map(row => row.updated(j, row(i)).updated(i, row(j))))
            )
        else None
      }

    def delete(column: String): ContactsCSV =
      columnOf(column).map { i =>
        copy(schema = schema.delete(i), content = content.map(row => row.take(i) ++ row.drop(i + 1)))
      }.getOrElse(self)

    def combine(column1: String, column2: String)(
      newColumn: String
    )(f: (String, String) => String): Option[ContactsCSV] =
      for {
        index1 <- columnOf(column1)
        index2 <- columnOf(column2)
        column = content.map(row => f(row(index1), row(index2)))
      } yield add(newColumn, column).delete(column1).delete(column2)
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
     * BONUS: EXERCISE 3
     *
     * Add an `protect` operator that returns a new schema mapping that
     * preserve the specified column names in the final result.
     */
    def protect(columnNames: Set[String]): SchemaMapping = ???
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
    def combine(leftColumn: String, rightColumn: String)(newName: String)(
      f: (String, String) => String
    ): SchemaMapping = ???

    /**
     * EXERCISE 5
     *
     * Add a constructor for `SchemaMapping` that moves the column of the
     * specified name to the jth position.
     */
    def relocate(column: String, j: Int): SchemaMapping = ???

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

    def checker: Checker[A]
  }
  object Question {
    final case class Text(question: String, checker: Checker[String]) extends Question[String]
    final case class MultipleChoice(question: String, choices: Vector[String], checker: Checker[Int])
        extends Question[Int]
    final case class TrueFalse(question: String, checker: Checker[Boolean]) extends Question[Boolean]
  }

  final case class QuizResult(correctPoints: Int, bonusPoints: Int, wrongPoints: Int, wrong: Vector[String]) {
    def totalPoints: Int = correctPoints - wrongPoints

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
    private def grade[A](f: String => A, checker: Checker[A]): QuizResult =
      scala.util.Try {
        val submittedAnswer = f(scala.io.StdIn.readLine())

        checker.isCorrect(submittedAnswer) match {
          case Left(string)  => QuizResult(0, 0, checker.points, Vector(string))
          case Right(string) => QuizResult(checker.points, 0, 0, Vector.empty)
        }
      }.getOrElse(QuizResult(0, 0, checker.points, Vector("The format of your answer was not recognized")))

    def apply[A](question: Question[A]): Quiz =
      Quiz { () =>
        import Question._

        println(question.question)

        question match {
          case Text(question, checker) => grade(identity(_), checker)
          case MultipleChoice(question, choices, checker) =>
            val choicePrintout = choices.zipWithIndex.map { case (c, i) => s"${i}. ${c}" }.mkString("\n")

            println("Your options are: \n" + choicePrintout)

            grade(_.toInt, checker)
          case TrueFalse(question, checker) => grade(_.toLowerCase().startsWith("t"), checker)
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

  final case class Checker[-A](points: Int, isCorrect: A => Either[String, Unit])
  object Checker {
    def isTrue(points: Int): Checker[Boolean] =
      Checker(points, if (_) Right(()) else Left("The correct answer is true"))
    def isFalse(points: Int): Checker[Boolean] =
      Checker(points, v => if (!v) Right(()) else Left("The correct answer is false"))

    def isMultipleChoice(points: Int)(choiceNumber: Int): Checker[Int] =
      Checker(points, v => if (v == choiceNumber) Right(()) else Left(s"The correct answer is ${choiceNumber}"))

    def isText(points: Int)(text: String): Checker[String] =
      Checker(points, v => if (v == text) Right(()) else Left(s"The correct answer is ${text}"))
  }

  /**
   * EXERCISE 7
   *
   * Extend the following quiz with an additional 3 questions, including a
   * tough bonus question; and if the user fails the bonus question, fallback
   * to a simpler bonus question with fewer bonus points.
   */
  lazy val exampleQuiz: Quiz =
    Quiz(Question.TrueFalse("Is coffee the best hot beverage on planet earth?", Checker.isTrue(10)))
}
