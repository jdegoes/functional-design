package net.degoes

/*
 * INTRODUCTION
 *
 * In Functional Design, type safety of data models can be greatly improved by
 * making them statically typed.
 */

/** EXECUTABLE - EXERCISE SET 1
  *
  * Consider an application (such as the spreadsheet example) that needs to calculate values in a
  * user-defined way.
  */
object executable_typed:
  trait Spreadsheet:
    def cols: Int
    def rows: Int

    def valueAt(col: Int, row: Int): CalculatedValue[Any]

    final def scan(range: Range): LazyList[Cell[Any]] =
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

  final case class Cell[A](col: Int, row: Int, contents: CalculatedValue[A])

  /** EXERCISE 1
    *
    * Design a data type called `CalculatedValue[A]`, whose type parameter `A` represents the type
    * of value dynamically computed from the spreadsheet.
    */
  final case class CalculatedValue[+A](eval: Spreadsheet => Either[String, A]):
    self =>

    /** EXERCISE 2
      *
      * Add an operator that returns a new `CalculatedValue` that is the negated version of this
      * one.
      */
    def unary_-[A1 >: A](using n: Numeric[A1]): CalculatedValue[A1] = 
      CalculatedValue(s => eval(s).map(n.negate))

    /** EXERCISE 3
      *
      * Add a binary operator `+` that returns a new `CalculatedValue` that is the sum of the two
      * calculated values.
      */
    def +[A1 >: A](that: CalculatedValue[A1])(using n: Numeric[A1]): CalculatedValue[A1] =
      CalculatedValue(s => 
        for 
          a <- self.eval(s)
          b <- that.eval(s)
        yield n.plus(a, b)
      )

    /** EXERCISE 4
      *
      * Add a binary operator `-` that returns a new `CalculatedValue` that is the difference of the
      * two calculated values.
      */
    def -[A1 >: A](that: CalculatedValue[A1])(using n: Numeric[A1]): CalculatedValue[A1] =
      CalculatedValue(s => 
        for 
          a <- self.eval(s)
          b <- that.eval(s)
        yield n.minus(a, b)
      )
  end CalculatedValue
  object CalculatedValue:

    /** EXERCISE 5
      *
      * Add a constructor that makes an `CalculatedValue` from a `Value`.
      */
    def const[Value](contents: Value): CalculatedValue[Value] = 
      CalculatedValue(_ => Right(contents))

    /** EXERCISE 6
      *
      * Add a constructor that provides access to the value of the specified cell, identified by
      * col/row.
      */
    def at(col: Int, row: Int): CalculatedValue[Any] = 
      CalculatedValue(s => 
        s.valueAt(col, row).eval(s)
      )
end executable_typed

/** EXECUTABLE - EXERCISE SET 2
  *
  * Consider an application (such as the spreadsheet example) that needs to calculate values in a
  * user-defined way.
  */
object declarative_typed:
  trait Spreadsheet:
    def cols: Int
    def rows: Int

    def valueAt(col: Int, row: Int): CalculatedValue[Any]

    final def scan(range: Range): LazyList[Cell[Any]] =
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

  final case class Cell[A](col: Int, row: Int, contents: CalculatedValue[A])

  /** EXERCISE 1
    *
    * Design a data type called `CalculatedValue[A]`, whose type parameter `A` represents the type
    * of value dynamically computed from the spreadsheet.
    */
  enum CalculatedValue[+A]:
    case Integer(value: Int) extends CalculatedValue[Int]
    case Str(value: String)  extends CalculatedValue[String]
    case Negate(value: CalculatedValue[A])(using Numeric[A]) extends CalculatedValue[A]
    case Add(left: CalculatedValue[A], right: CalculatedValue[A])(using Numeric[A]) extends CalculatedValue[A]
    case Subtract(left: CalculatedValue[A], right: CalculatedValue[A])(using Numeric[A]) extends CalculatedValue[A]
    case At(col: Int, row: Int) extends CalculatedValue[Any]

    def self = this

    /** EXERCISE 2
      *
      * Add an operator that returns a new `CalculatedValue` that is the negated version of this
      * one.
      */
    def unary_-[A1 >: A](using Numeric[A1]): CalculatedValue[A1] = 
      Negate(self: CalculatedValue[A1])

    /** EXERCISE 3
      *
      * Add a binary operator `+` that returns a new `CalculatedValue` that is the sum of the two
      * calculated values.
      */
    def +[A1 >: A](that: CalculatedValue[A1])(using Numeric[A1]): CalculatedValue[A1] =
      Add(self, that)

    /** EXERCISE 4
      *
      * Add a binary operator `-` that returns a new `CalculatedValue` that is the difference of the
      * two calculated values.
      */
    def -[A1 >: A](that: CalculatedValue[A1])(using Numeric[A1]): CalculatedValue[A1] =
      Subtract(self, that)
  end CalculatedValue
  object CalculatedValue:

    /** EXERCISE 5
      *
      * Add a constructor that makes an `CalculatedValue` from a `Value`.
      */
    def const[A](contents: A)(using cv: IsCellValue[A]): CalculatedValue[A] = 
      contents.toCalculatedValue

    /** EXERCISE 6
      *
      * Add a constructor that provides access to the value of the specified cell, identified by
      * col/row.
      */
    def at(col: Int, row: Int): CalculatedValue[Any] = 
      At(col, row)

  import CalculatedValue.*

  enum IsCellValue[A]:
    case StringIsCellValue extends IsCellValue[String]
    case IntIsCellValue extends IsCellValue[Int]

    extension (value: A) def toCalculatedValue: CalculatedValue[A] = 
      this match
        case StringIsCellValue => Str(value)
        case IntIsCellValue => Integer(value)

  object IsCellValue:
    given IsCellValue[String] = StringIsCellValue
    given IsCellValue[Int] = IntIsCellValue

  def calculate[A](s: Spreadsheet, expr: CalculatedValue[A]): A =
    ???
end declarative_typed

/** PARSERS - GRADUATION PROJECT
  */
object parser:
  type Error = String
  type Input = String

  // `Parser[A]` is a model of a series of parse operations that consume
  // characters and ultimately use the consumed input construct a value of
  // type `A`.
  enum Parser[+A]:
    case OneChar                                                         extends Parser[Char]
    case Map[A, B](parser: Parser[A], f: A => B)                         extends Parser[B]
    case Repeat[A](value: Parser[A], min: Option[Int], max: Option[Int]) extends Parser[List[A]]
    case Succeed(value: A)
    case Fail(error: String)
    case OrElse(left: Parser[A], right: Parser[A])
    case Sequence[A, B](left: Parser[A], right: Parser[B])               extends Parser[(A, B)]

    def self = this

    def atLeast(n: Int): Parser[List[A]] = Parser.Repeat(self, Some(n), None)

    def atMost(n: Int): Parser[List[A]] = Parser.Repeat(self, None, Some(n))

    def between(min: Int, max: Int): Parser[List[A]] = Parser.Repeat(self, Some(min), Some(max))

    def map[B](f: A => B): Parser[B] = Parser.Map(self, f)

    def ~>[B](that: Parser[B]): Parser[B] = (self ~ that).map(_._2)

    def <~[B](that: Parser[B]): Parser[A] = (self ~ that).map(_._1)

    def ~[B](that: Parser[B]): Parser[(A, B)] = Parser.Sequence(self, that)

    def |[A1 >: A](that: Parser[A1]): Parser[A1] = Parser.OrElse(self, that)

    def * : Parser[List[A]] = Parser.Repeat(self, None, None)

    def + : Parser[List[A]] = atLeast(1)
  end Parser
  object Parser:

    /** EXERCISE 1
      *
      * Add a constructor that models the production of the specified value (of any type at all),
      * without consuming any input.
      *
      * NOTE: Be sure to modify the `parse` method below, so that it can handle the new operation.
      */
    def succeed[A](value: A): Parser[A] = Parser.Succeed(value)

    /** EXERCISE 2
      *
      * Add a constructor that models failure with a string error message.
      *
      * NOTE: Be sure to modify the `parse` method below, so that it can handle the new operation.
      */
    def fail(error: String): Parser[Nothing] = Parser.Fail(error)

    /** EXERCISE 3
      *
      * Add an operator that can try one parser, but if that fails, try another parser.
      *
      * NOTE: Be sure to modify the `parse` method below, so that it can handle the new operation.
      */
    def fallback[A](first: Parser[A], second: Parser[A]): Parser[A] = Parser.OrElse(first, second)

    /** EXERCISE 4
      *
      * Add an operator that parses one thing, and then parses another one, in sequence, producing a
      * tuple of their results.
      *
      * NOTE: Be sure to modify the `parse` method below, so that it can handle the new operation.
      */
    def sequence[A, B](first: Parser[A], second: Parser[B]): Parser[(A, B)] = Parser.Sequence(first, second)
  end Parser

  import Parser.*

  extension [A] (parser: Parser[A]) def parse(input: Input): Either[Error, (Input, A)] =
    parser match
      case Succeed(value) => Right(input -> value)
      
      case Fail(message) => Left(message)

      case OrElse(left, right) => 
        left.parse(input) match
          case Left(_) => right.parse(input)
          case other   => other

      case Sequence(left, right) => 
        for 
          (input, a) <- left.parse(input)
          (input, b) <- right.parse(input)
        yield (input, (a, b))

      case OneChar =>
        input.headOption
          .map((a: Char) => Right(input.drop(1) -> a))
          .getOrElse(Left("Expected a character but found an empty string"))

      case Map(parser, f) =>
        parser.parse(input).map:
          case (input, a) => (input, f(a))

      case repeat: Repeat[a] =>
        val min = repeat.min.getOrElse(0)
        val max = repeat.max.getOrElse(Int.MaxValue)

        (min to max)
          .foldLeft[Either[Error, (Input, List[a])]](Right((input, Nil))):
            case (e @ Left(_), _) => e

            case (Right((input, as)), _) =>
              repeat.value.parse(input) match
                case Left(error)       => if as.length >= min then Right((input, as)) else Left(error)
                case Right((input, a)) => Right((input, a :: as))
          .map:
            case (input, as) => (input, as.reverse)
end parser

object declarative_parser:
  enum Parser[+A]:
    case Consume extends Parser[Char]
    case Succeed(value: A)
    case Fail(error: String)
    case Attempt(value: Parser[A]) extends Parser[Either[String, A]]
    case Sequence[A, B](left: Parser[A], andThen: A => Parser[B]) extends Parser[B]

    def self = this 

    def attempt: Parser[Either[String, A]] = Attempt(self)

    def map[B](f: A => B): Parser[B] = Sequence(self, (a: A) => Succeed(f(a)))

    def flatMap[B](f: A => Parser[B]): Parser[B] = Sequence(self, f)

    def | [A1 >: A](that: => Parser[A1]): Parser[A1] = 
      self.attempt.flatMap:
        case Left(_) => that 
        case Right(value) => Succeed(value)

    def ~ [B](that: Parser[B]): Parser[(A, B)] = self.flatMap(a => that.map(b => (a, b)))

    def ~> [B](that: Parser[B]): Parser[B] = (self ~ that).map(_._2)

    def <~ [B](that: Parser[B]): Parser[A] = (self ~ that).map(_._1)

  val never = Parser.Fail("Uh oh!")
    