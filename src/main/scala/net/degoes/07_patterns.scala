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

/**
 * UNTYPED FUNCTIONAL DOMAINS - EXERCISE SET 2
 */
object untyped {
  lazy val nameValidation =
    JsonValidation.start.field("name").string("""\w+(\s+(\w|\s)+)+""".r)

  sealed trait JsonValidation { self =>
    def element(index: Int): JsonValidation = JsonValidation.DescendElement(self, index)

    def elements: JsonValidation = JsonValidation.DescendElements(self)

    def field(name: String): JsonValidation = JsonValidation.DescendField(self, name)

    def number: JsonValidation = JsonValidation.ValidateNumber(self, None, None)

    def numberBetween(min: BigDecimal, max: BigDecimal): JsonValidation =
      JsonValidation.ValidateNumber(self, Some(min), Some(max))

    def string(regex: Regex): JsonValidation = JsonValidation.ValidateString(self, regex)

    /**
     * EXERCISE
     *
     * Design a binary operator with the meaning of sequential composition.
     * For example, `JsonValidation.start.field("address") ++
     * JsonValidation.start.field("street")` would first validate that a field
     * called `address` exists, and then would descend into that field value
     * to validate that a field called `street` exists within it.
     */
    def ++(that: JsonValidation): JsonValidation = ???

    /**
     * EXERCISE
     *
     * Design a binary operator with the meaning of parallel composition.
     * The meaning of `a && b` should be that `a` is validated, and also `b`
     * is validated (starting from the root of the JSON object).
     */
    def &&(that: JsonValidation): JsonValidation = ???

    /**
     * EXERCISE
     *
     * Design a binary operator with the meaning of fallback. The meaning of
     * `a || b` should be that `a` is validated, but if the validation fails,
     * then `b` is validated (starting from the root of the JSON object).
     */
    def ||(that: JsonValidation): JsonValidation = ???
  }
  object JsonValidation {
    case object Start                                                   extends JsonValidation
    final case class DescendField(parent: JsonValidation, name: String) extends JsonValidation
    final case class DescendElement(parent: JsonValidation, index: Int) extends JsonValidation
    final case class DescendElements(parent: JsonValidation)            extends JsonValidation
    final case class ValidateNumber(parent: JsonValidation, min: Option[BigDecimal], max: Option[BigDecimal])
        extends JsonValidation
    final case class ValidateString(parent: JsonValidation, pattern: Regex) extends JsonValidation

    def start: JsonValidation = Start
  }

  sealed trait Json
  object Json {
    final case class Object(fields: Map[String, Json]) extends Json
    final case class Array(elements: List[Json])       extends Json
    final case class String(value: String)             extends Json
    final case class Number(value: BigDecimal)         extends Json
    final case class Boolean(value: Boolean)           extends Json
    case object Null                                   extends Json
  }

  final case class JsonValidator(validation: JsonValidation) {

    /**
     * EXERCISE
     *
     * Implement the following executor which validates JSON.
     */
    def validateWith(json: Json): Either[String, Unit] = ???
  }
}

/**
 * TYPED FUNCTIONAL DOMAINS - EXERCISE SET 2
 */
object typed {
  sealed trait Baked[+A]
  object Baked {
    final case class Burnt[A](value: A)         extends Baked[A]
    final case class CookedPerfect[A](value: A) extends Baked[A]
    final case class Undercooked[A](value: A)   extends Baked[A]
  }

  sealed trait Ingredient
  object Ingredient {
    final case class Eggs(number: Int)        extends Ingredient
    final case class Sugar(amount: Double)    extends Ingredient
    final case class Flour(amount: Double)    extends Ingredient
    final case class Cinnamon(amount: Double) extends Ingredient
  }

  sealed trait Recipe[+A] { self =>

    /**
     * Uses all the ingredients in a recipe by baking them to produce a
     * baked result.
     */
    def bake(temp: Int, time: Int): Recipe[Baked[A]] = Recipe.Bake(self, temp, time)

    /**
     * EXERCISE 1
     *
     * Implement a `both` operation that allows combining two recipes into
     * one, producing both items in a tuple.
     *
     * NOTE: Be sure to update the `bake` method below so that you can make
     * recipes that use your new operation.
     */
    def both[B](that: Recipe[B]): Recipe[(A, B)] = ???

    /**
     * EXERCISE 2
     *
     * Implement a `either` operation that allows trying a backup recipe,
     * in case this recipe ends in disaster.
     *
     * NOTE: Be sure to update the `bake` method below so that you can make
     * recipes that use your new operation.
     */
    def either[B](that: Recipe[B]): Recipe[Either[A, B]] = ???

    /**
     * EXERCISE 3
     *
     * Implement a `map` operation that allows changing what a recipe produces.
     *
     * NOTE: Be sure to update the `bake` method below so that you can make
     * recipes that use your new operation.
     */
    def map[B](f: A => B): Recipe[B] = ???

    /**
     * EXERCISE 4
     *
     * Implement a `flatMap` operation that allows deciding which recipe to
     * make after this recipe has produced its item.
     *
     * NOTE: Be sure to update the `bake` method below so that you can make
     * recipes that use your new operation.
     */
    def flatMap[B](f: A => Recipe[B]): Recipe[B] = ???
  }
  object Recipe {
    case object Disaster                                   extends Recipe[Nothing]
    final case class AddIngredient(ingredient: Ingredient) extends Recipe[Unit]
    final case class Bake[A](recipe: Recipe[A], temp: Int, time: Int) extends Recipe[Baked[A]] {
      type Type = A
    }

    def addIngredient(ingredient: Ingredient): Recipe[Unit] = AddIngredient(ingredient)

    def disaster: Recipe[Nothing] = Disaster
  }
  import Recipe._

  def bake[Out](recipe: Recipe[Out]): Out = {
    def loop[A](ingredients: Vector[Ingredient], recipe: Recipe[A]): (Vector[Ingredient], A) =
      recipe match {
        case Disaster                  => throw new Exception("Uh no, utter disaster!")
        case AddIngredient(ingredient) => (ingredients :+ ingredient, ())
        case bake @ Bake(recipe, temp, time) =>
          val (leftover, a) = loop[bake.Type](ingredients, recipe)

          println(s"Baking ${a} for ${time} minutes at ${temp} temperature")
          println("Ingredients: ")
          println(ingredients.mkString("\n"))

          if (time * temp < 1000) (Vector(), Baked.Undercooked(a))
          else if (time * temp > 6000) (Vector(), Baked.Burnt(a))
          else (Vector(), Baked.CookedPerfect(a))
      }

    val (leftover, a) = loop(Vector(), recipe)

    println(s"Leftover ingredients: ${leftover}")

    a
  }
  final case class Cake(ingredients: List[Ingredient])

  /**
   * EXERCISE 5
   *
   * Make a recipe that will produced a baked cake or other food of your choice!
   */
  lazy val recipe: Recipe[Baked[Cake]] = ???
}
