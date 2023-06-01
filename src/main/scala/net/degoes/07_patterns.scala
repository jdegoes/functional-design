package net.degoes

import scala.util.matching.Regex

/*
 * INTRODUCTION
 *
 * In Functional Design, operators that transform and compose values in a
 * domain often fall into pre-existing patterns.
 *
 * In this section, you'll learn to identify these patterns, first in
 * untyped functional domains, and then in typed functional domains.
 *
 */

/** UNTYPED FUNCTIONAL DOMAINS - EXERCISE SET 2
  */
object untyped:
  val nameRegex = """\w+(\s+(\w|\s)+)+""".r

  lazy val nameValidation =
    JsonValidation.start.field("name").string(nameRegex)

  enum JsonValidation:
    case Start
    case DescendField(parent: JsonValidation, name: String)
    case DescendElement(parent: JsonValidation, index: Int)
    case DescendElements(parent: JsonValidation)
    case ValidateNumber(
      parent: JsonValidation,
      min: Option[BigDecimal],
      max: Option[BigDecimal]
    ) extends JsonValidation
    case ValidateString(parent: JsonValidation, pattern: Regex)
    case Both(left: JsonValidation, right: JsonValidation)
    case OneOf(left: JsonValidation, right: JsonValidation)

    def self = this

    def element(index: Int): JsonValidation = JsonValidation.DescendElement(self, index)

    def elements: JsonValidation = JsonValidation.DescendElements(self)

    def field(name: String): JsonValidation = JsonValidation.DescendField(self, name)

    def number: JsonValidation = JsonValidation.ValidateNumber(self, None, None)

    def numberBetween(min: BigDecimal, max: BigDecimal): JsonValidation =
      JsonValidation.ValidateNumber(self, Some(min), Some(max))

    def string(regex: Regex): JsonValidation = JsonValidation.ValidateString(self, regex)

    /** EXERCISE
      *
      * Design a binary operator with the meaning of sequential composition. For example,
      * `JsonValidation.start.field("address") ++ JsonValidation.start.field("street")` would first
      * validate that a field called `address` exists, and then would descend into that field value
      * to validate that a field called `street` exists within it.
      */
    def ++(that: JsonValidation): JsonValidation = 
      that match
        case Start => self 
        case DescendField(parent, name) => DescendField(self ++ parent, name)
        case DescendElement(parent, index) => DescendElement(self ++ parent, index)
        case DescendElements(parent) => DescendElements(self ++ parent)
        case ValidateNumber(parent, min, max) => ValidateNumber(self ++ parent, min, max)
        case ValidateString(parent, pattern) => ValidateString(self ++ parent, pattern)
        case Both(left, right) => Both(self ++ left, self ++ right)
        case OneOf(left, right) => OneOf(self ++ left, self ++ right)

    /** EXERCISE
      *
      * Design a binary operator with the meaning of parallel composition. The meaning of `a && b`
      * should be that `a` is validated, and also `b` is validated (starting from the root of the
      * JSON object).
      */
    def &&(that: JsonValidation): JsonValidation = Both(self, that)

    /** EXERCISE
      *
      * Design a binary operator with the meaning of fallback. The meaning of `a || b` should be
      * that `a` is validated, but if the validation fails, then `b` is validated (starting from the
      * root of the JSON object).
      */
    def ||(that: JsonValidation): JsonValidation = OneOf(self, that)
  end JsonValidation
  object JsonValidation:
    def start: JsonValidation = Start

  enum Json:
    case Object(fields: Map[Predef.String, Json])
    case Array(elements0: List[Json])
    case String(value: Predef.String)
    case Number(value: BigDecimal)
    case Boolean(value: Boolean)
    case Null

    def ++ (that: Json): Json = 
      this match
        case Object(fields) => that match
          case Object(fields2) => Object(fields ++ fields2)
          case _ => this
        case Array(elements) => that match
          case Array(elements2) => Array(elements ++ elements2)
          case _ => this
        case _ => this

    def field(name: Predef.String): Either[Predef.String, Json] = 
      this match
        case Object(fields) => fields.get(name).toRight(s"Field ${name} not found")
        case _ => Left(s"Field ${name} not found")

    def element(index: Int): Either[Predef.String, Json] = 
      this match
        case Array(elements) => elements.lift(index).toRight(s"Element ${index} not found")
        case _ => Left(s"Element ${index} not found")

    def elements: Either[Predef.String, List[Json]] = 
      this match
        case Array(elements) => Right(elements)
        case _ => Left(s"Expected array")

    def number: Either[Predef.String, BigDecimal] = 
      this match
        case Number(value) => Right(value)
        case _ => Left(s"Expected number")

    def string: Either[Predef.String, Predef.String] =
      this match
        case String(value) => Right(value)
        case _ => Left(s"Expected string")

  def forEachEither[A, E, B](list: List[A])(f: A => Either[E, B]): Either[E, List[B]] = 
    list.foldRight(Right(List.empty): Either[E, List[B]]) { (a, acc) =>
      for
        bs <- acc
        b <- f(a)
      yield b :: bs
    }

  def checkRange(b: BigDecimal, min: Option[BigDecimal], max: Option[BigDecimal]): Either[String, Unit] = 
    for
      _ <- min.fold(Right(()))(min => if b >= min then Right(()) else Left(s"Number ${b} is less than ${min}"))
      _ <- max.fold(Right(()))(max => if b <= max then Right(()) else Left(s"Number ${b} is greater than ${max}"))
    yield ()

  /** EXERCISE
    *
    * Implement the following executor which validates JSON.
    */
  extension (validation: JsonValidation) def validate(json: Json): Either[String, Json] = 
    validation match
      case JsonValidation.Start => Right(json)

      case JsonValidation.DescendField(parent, name) =>
        for
          json <- json.field(name)
          json <- parent.validate(json)
        yield json

      case JsonValidation.DescendElement(parent, index) =>
        for
          json <- json.element(index)
          json <- parent.validate(json)
        yield json

      case JsonValidation.DescendElements(parent) =>
        for
          jsons <- json.elements
          jsons <- forEachEither(jsons)(parent.validate(_))
        yield Json.Array(jsons)

      case JsonValidation.ValidateNumber(parent, min, max) =>
        for 
          json <- parent.validate(json)
          number <- json.number
          _ <- checkRange(number, min, max)
        yield json

      case JsonValidation.ValidateString(parent, pattern) =>
        for
          json <- parent.validate(json)
          string <- json.string
          _ <- if !pattern.matches(string) then Left(s"String ${string} does not match pattern ${pattern}") else Right(())
        yield json

      case JsonValidation.Both(left, right) =>
        for
          leftJson  <- left.validate(json)
          rightJson <- right.validate(json)
        yield leftJson ++ rightJson

      case JsonValidation.OneOf(left, right) =>
        left.validate(json) match
          case Right(json) => Right(json)
          case Left(_) => right.validate(json)
    
  @main
  def test6 = 
    import JsonValidation.start 

    val personJson = 
      Json.Object(Map(
        "name" -> Json.String("John Doe"),
        "age" -> Json.Number(BigDecimal(42))
      ))

    // {"name": "John Doe", "age": 42}
    val personValidator = 
      start.field("name").string(nameRegex) && start.field("age").number

    println(personValidator.validate(personJson))
end untyped

/** TYPED FUNCTIONAL DOMAINS - EXERCISE SET 2
  */
object typed:
  enum Baked[+A]:
    case Burnt(value: A)
    case CookedPerfect(value: A)
    case Undercooked(value: A)

  enum Ingredient:
    case Eggs(number: Int)
    case Sugar(amount: Double)
    case Flour(amount: Double)
    case Cinnamon(amount: Double)

  enum Recipe[+A]:
    case Disaster                                      extends Recipe[Nothing]
    case AddIngredient(ingredient: Ingredient)         extends Recipe[Unit]
    case Bake(recipe: Recipe[A], temp: Int, time: Int) extends Recipe[Baked[A]]
    case Fallback(left: Recipe[A], right: Recipe[A])
    case Done(value: A)
    case FlatMap[A, B](recipe: Recipe[A], f: A => Recipe[B]) extends Recipe[B]

    def self = this

    /** Uses all the ingredients in a recipe by baking them to produce a baked result.
      */
    def bake(temp: Int, time: Int): Recipe[Baked[A]] = Recipe.Bake(self, temp, time)

    /** EXERCISE 1
      *
      * Implement a `both` operation that allows combining two recipes into one, producing both
      * items in a tuple.
      *
      * NOTE: Be sure to update the `bake` method below so that you can make recipes that use your
      * new operation.
      */
    def both[B](that: Recipe[B]): Recipe[(A, B)] = 
      self.flatMap(a => that.map(b => (a, b)))

    /** EXERCISE 2
      *
      * Implement a `either` operation that allows trying a backup recipe, in case this recipe ends
      * in disaster.
      *
      * NOTE: Be sure to update the `bake` method below so that you can make recipes that use your
      * new operation.
      */
    def either[B](that: Recipe[B]): Recipe[Either[A, B]] =
      Fallback(self.map(Left(_)), that.map(Right(_)))

    /** EXERCISE 3
      *
      * Implement a `map` operation that allows changing what a recipe produces.
      *
      * NOTE: Be sure to update the `bake` method below so that you can make recipes that use your
      * new operation.
      */
    def map[B](f: A => B): Recipe[B] = self.flatMap(a => Recipe.done(f(a)))

    /** EXERCISE 4
      *
      * Implement a `flatMap` operation that allows deciding which recipe to make after this recipe
      * has produced its item.
      *
      * NOTE: Be sure to update the `bake` method below so that you can make recipes that use your
      * new operation.
      */
    def flatMap[B](f: A => Recipe[B]): Recipe[B] = 
      FlatMap(self, f)
  end Recipe
  object Recipe:
    def done[A](value: A): Recipe[A] = Recipe.Done(value)

    def addIngredient(ingredient: Ingredient): Recipe[Unit] = AddIngredient(ingredient)

    def disaster: Recipe[Nothing] = Disaster
  import Recipe.*

  def bake[Out](recipe: Recipe[Out]): Out =
    def loop[A](ingredients: Vector[Ingredient], recipe: Recipe[A]): (Vector[Ingredient], A) =
      recipe match
        case FlatMap(recipe, f) =>
          val (leftover, a) = loop(ingredients, recipe)
          loop(leftover, f(a))
        case Done(value)                    => (ingredients, value)
        case Fallback(left, right)          =>
          try loop(ingredients, left)
          catch case _: Exception => loop(ingredients, right)
        case Disaster                        => throw new Exception("Uh no, utter disaster!")
        case AddIngredient(ingredient)       => (ingredients :+ ingredient, ())
        case bake @ Bake(recipe, temp, time) =>
          val (leftover, a) = loop(ingredients, recipe)

          println(s"Baking ${a} for ${time} minutes at ${temp} temperature")
          println("Ingredients: ")
          println(ingredients.mkString("\n"))

          if time * temp < 1000 then (Vector(), Baked.Undercooked(a))
          else if time * temp > 6000 then (Vector(), Baked.Burnt(a))
          else (Vector(), Baked.CookedPerfect(a))

    val (leftover, a) = loop(Vector(), recipe)

    println(s"Leftover ingredients: ${leftover}")

    a
  end bake
  final case class Cake(ingredients: List[Ingredient])

  /** EXERCISE 5
    *
    * Make a recipe that will produced a baked cake or other food of your choice!
    */
  lazy val recipe: Recipe[Baked[Cake]] = ???
end typed

object stack:
  enum StackVM[T <: Tuple]:
    case Empty                                           extends StackVM[EmptyTuple]
    case Push[A, T <: Tuple](value: A, prev: StackVM[T]) extends StackVM[A *: T]
    case Add[T <: Tuple, T2 <: Tuple](prev: StackVM[T])(using T <:< Int *: Int *: T2)
        extends StackVM[Int *: T2]

    def self = this

    def ++ [T2 <: Tuple](that: StackVM[T2]): StackVM[Tuple.Concat[T, T2]] = 
      ???

    def push[A](a: A): StackVM[A *: T] = StackVM.Push(a, self)

    def add[T2 <: Tuple](using T <:< Int *: Int *: T2): StackVM[Int *: T2] = Add(self)

    def run: T =
      def loop(op: StackVM[?], stack: Tuple): Tuple =
        op match
          case Empty             => stack
          case Push(value, prev) => loop(prev, value *: stack)
          case Add(prev)         =>
            loop(prev, stack) match
              case x1 *: x2 *: xs =>
                (x1.asInstanceOf[Int] + x2.asInstanceOf[Int]) *: xs

              case xs => throw new IllegalStateException(s"Uh oh: ${xs}")

      loop(self, EmptyTuple).asInstanceOf[T]
  end StackVM

  private def unsafePop(stack: StackVM[?]): StackVM[Tuple] = 
    stack match
      case StackVM.Empty => throw new IllegalStateException("Stack is empty")   
      case StackVM.Push(value, prev) => prev.asInstanceOf[StackVM[Tuple]]
      case StackVM.Add(prev) =>
        unsafePop(unsafePop(prev)).asInstanceOf[StackVM[Tuple]]    

  extension [H, T <: Tuple] (stack: StackVM[H *: T]) def pop: StackVM[T] = 
    unsafePop(stack).asInstanceOf[StackVM[T]]
    

  def empty: StackVM[EmptyTuple] = StackVM.Empty

  @main
  def example =
    empty.push(1).push("abc").pop 

    println(empty.push(1).push(2).add.run)

end stack
