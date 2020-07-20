package net.degoes

/*
 * INTRODUCTION
 *
 * In Functional Design, type safety of data models can be greatly improved by
 * using so-called generalized algebraic data types.
 *
 * In this section, you'll review GADTs with a focus on functional models.
 *
 */

/**
 * EXPRESSIONS - EXERCISE SET 1
 *
 * Consider an application (such as the spreadsheet example) that needs to
 * calculate values in a user-defined way.
 */
object expr {
  sealed trait CalculatedValue[+A]
  object CalculatedValue {
    final case class Integer(value: Int) extends CalculatedValue[Int]
    final case class Str(value: String)  extends CalculatedValue[String]

    /**
     * EXERCISE 1
     *
     * Add an operator that adds two integer expressions, yielding an integer
     * expression.
     *
     * NOTE: Be sure to modify the `calculate` method below, so that it can
     * handle the new operation.
     */
    final case class Add()

    /**
     * EXERCISE 2
     *
     * Add an operator that subtracts an integer from another integer expression,
     * yielding an integer expression.
     *
     * NOTE: Be sure to modify the `calculate` method below, so that it can
     * handle the new operation.
     */
    final case class Subtract()

    /**
     * EXERCISE 3
     *
     * Add an operator that multiplies two integer expressions, yielding an
     * integer expression.
     *
     * NOTE: Be sure to modify the `calculate` method below, so that it can
     * handle the new operation.
     */
    final case class Multiply()

    /**
     * EXERCISE 4
     *
     * Add an operator that concatenates two strings, yielding a string
     * expression.
     *
     * NOTE: Be sure to modify the `calculate` method below, so that it can
     * handle the new operation.
     */
    final case class Concat()

    /**
     * EXERCISE 5
     *
     * Add an operator that determines if a string starts with a specified
     * prefix, yielding a boolean expression.
     *
     * NOTE: Be sure to modify the `calculate` method below, so that it can
     * handle the new operation.
     */
    final case class StartsWith()
  }

  import CalculatedValue._

  def calculate[A](expr: CalculatedValue[A]): A =
    expr match {
      case Integer(v) => v
      case Str(v)     => v
    }
}

/**
 * PARSERS - EXERCISE SET 2
 */
object parser {
  type Error = String
  type Input = String

  // `Parser[A]` is a model of a series of parse operations that consume
  // characters and ultimately use the consumed input construct a value of
  // type `A`.
  sealed trait Parser[+A] { self =>
    def atLeast(n: Int): Parser[List[A]] = Parser.Repeat(self, Some(n), None)

    def atMost(n: Int): Parser[List[A]] = Parser.Repeat(self, None, Some(n))

    def between(min: Int, max: Int): Parser[List[A]] = Parser.Repeat(self, Some(min), Some(max))

    def * : Parser[List[A]] = Parser.Repeat(self, None, None)

    def + : Parser[List[A]] = atLeast(1)
  }
  object Parser {
    final case object OneChar extends Parser[Char]

    final case class Repeat[A](value: Parser[A], min: Option[Int], max: Option[Int]) extends Parser[List[A]]

    /**
     * EXERCISE 1
     *
     * Add a constructor that models the production of the specified value (of
     * any type at all), without consuming any input.
     *
     * NOTE: Be sure to modify the `parse` method below, so that it can
     * handle the new operation.
     */
    final case class Succeed()

    /**
     * EXERCISE 2
     *
     * Add a constructor that models failure with a string error message.
     *
     * NOTE: Be sure to modify the `parse` method below, so that it can
     * handle the new operation.
     */
    final case class Fail()

    /**
     * EXERCISE 3
     *
     * Add an operator that can try one parser, but if that fails, try
     * another parser.
     *
     * NOTE: Be sure to modify the `parse` method below, so that it can
     * handle the new operation.
     */
    final case class OrElse()

    /**
     * EXERCISE 4
     *
     * Add an operator that parses one thing, and then parses another one,
     * in sequence, producing a tuple of their results.
     *
     * NOTE: Be sure to modify the `parse` method below, so that it can
     * handle the new operation.
     */
    final case class Sequence[A, B]()
  }

  import Parser._

  def parse[A](parser: Parser[A], input: Input): Either[Error, (Input, A)] =
    parser match {
      case OneChar =>
        input.headOption
          .map((a: Char) => Right(input.drop(1) -> a))
          .getOrElse(Left("Expected a character but found an empty string"))

      case repeat: Repeat[a] =>
        val min = repeat.min.getOrElse(0)
        val max = repeat.max.getOrElse(Int.MaxValue)

        (min to max)
          .foldLeft[Either[Error, (Input, List[a])]](Right((input, Nil))) {
            case (e @ Left(_), _) => e

            case (Right((input, as)), _) =>
              parse[a](repeat.value, input) match {
                case Left(error)       => if (as.length >= min) Right((input, as)) else Left(error)
                case Right((input, a)) => Right((input, a :: as))
              }
          }
          .map {
            case (input, as) => (input, as.reverse)
          }
    }
}
