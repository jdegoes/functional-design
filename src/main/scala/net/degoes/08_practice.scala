package net.degoes

/*
 * INTRODUCTION
 *
 * In Functional Design, the most powerful approaches utilize typed
 * declarative encodings to model solutions to the domain problem, exposing a
 * composable, expressive, and orthogonal set of primitive constructors and
 * operators, upon which they add derived constructors and operators to bridge
 * gaps between the foundation and common problems in the domain.
 *
 * In this section, you'll tie together everything you've learned to develop
 * hands-on skills in applied functional design.
 */

/** RESOURCES - EXERCISE SET 1
  *
  * Consider resource-safe applications.
  */
object resources:

  /** EXERCISE 1
    *
    * Design a data type that can model a resource. The data type should be equipped with operators,
    * which allow transforming and composing resources. You can use either the declarative or
    * executable encodings.
    */
  trait Resource[+A]
  object Resource

/** CALENDER SCHEDULING APP - EXERCISE SET 2
  */
object calendar:
  final case class HourOfDay(value: Int):
    def to(that: HourOfDay): LazyList[HourOfDay] =
      (value to that.value).to(LazyList).map(HourOfDay(_))

    def until(that: HourOfDay): LazyList[HourOfDay] =
      (value until that.value).to(LazyList).map(HourOfDay(_))
  object HourOfDay:
    val min = HourOfDay(0)
    val max = HourOfDay(24)

  enum DayOfWeek:
    case Sunday
    case Monday
    case Tuesday
    case Wednesday
    case Thursday
    case Friday
    case Saturday

  /** EXERCISE 1
    *
    * Explore the structure of `CalendarAppointment` by deciding what composable, orthogonal
    * operations to add to the data type.
    */
  final case class CalendarAppointment(start: HourOfDay, end: HourOfDay):
    self =>
    def length: Int = end.value - start.value
  object CalendarAppointment:
    val empty: CalendarAppointment = CalendarAppointment(HourOfDay(0), HourOfDay(0))
    val full: CalendarAppointment  = CalendarAppointment(HourOfDay(0), HourOfDay(24))

  /** EXERCISE 2
    *
    * Explore the structure of `DailySchedule` by deciding what composable, orthogonal operations to
    * add to the data type.
    */
  final case class DailySchedule(set: Set[CalendarAppointment]):
    self =>
  object DailySchedule:
    val empty: DailySchedule = DailySchedule(Set())

  /** EXERCISE 3
    *
    * Explore the structure of `MonthlySchedule` by deciding what composable, orthogonal operations
    * to add to the data type.
    */
  final case class MonthlySchedule(daysOfMonth: Vector[DailySchedule]):
    self =>
  object MonthlySchedule:
    val empty: MonthlySchedule = MonthlySchedule(Vector())

  final case class Person(name: String)

  /** EXERCISE 4
    *
    * Using the operators you build, express a solution to the following problem: find all the free
    * times that a group of friends can virtually meet for the specified number of hours.
    */
  def findFreeTimes(lengthInHours: Int, friends: Map[Person, MonthlySchedule]): MonthlySchedule =
    ???
end calendar

/** CMS - EXERCISE SET 3
  *
  * Consider a content-management system.
  */
object cms:

  /** EXERCISE 1
    *
    * Add whatever transformations and combinations have well-defined semantics.
    */
  enum Html:
    case Zero
    case One(tagName: String, attributes: Map[String, String], children: Html)
    case Many(elements: List[Html])

    def self = this

    final def isEmpty: Boolean = self match
      case Html.Zero                => true
      case Html.One(_, _, children) => false
      case Html.Many(elements)      => elements.forall(_.isEmpty)

    final def childList: List[Html] = self match
      case Html.Zero                => Nil
      case Html.One(_, _, children) => children.childList
      case Html.Many(elements)      => elements.flatMap(_.childList)

  final case class User(email: String)

  trait PageContext:
    def url: java.net.URL
  trait UserContext:
    def user: User

  /** EXERCISE 2
    *
    * Add whatever transformations and combinations have well-defined semantics.
    */
  enum Component[-Context, -State]:
    case Leaf(render0: (Context, State) => Html)

    def render(context: Context, state: State): Html =
      this match
        case Leaf(render0) => render0(context, state)
  object Component:

    def make[Context, State](render: (Context, State) => Html): Component[Context, State] = Leaf(
      render
    )

    /** EXERCISE 3
      *
      * Add some example components.
      */
    val components = ???
end cms

/** JSON VALIDATION - EXERCISE SET 4
  *
  * Consider a domain where incoming JSON documents are stored into a NoSQL database, but before
  * being stored, they must be validated by flexible rules. If validation fails, descriptive error
  * messages must be generated that allow clients of the JSON endpoint to fix the issues with their
  * data.
  */
object input_validation:
  enum Json:
    case Null
    case Bool(value: Boolean)
    case Number(value: BigDecimal)
    case Text(value: String)
    case Sequence(value: List[Json])
    case Object(value: Map[String, Json])

    /** EXERCISE 1
      *
      * Implement a method to retrieve the JSON value at the specified path, or fail with a
      * descriptive error message.
      */
    def get(path: JsonPath): Either[String, Json] = ???

  enum JsonPath:
    case Identity
    case Field(parent: JsonPath, name: String)
    case Index(parent: JsonPath, index: Int)

    def self = this

    def +(that: JsonPath): JsonPath =
      that match
        case JsonPath.Identity             => self
        case JsonPath.Field(parent, name)  => JsonPath.Field(self + parent, name)
        case JsonPath.Index(parent, index) => JsonPath.Index(self + parent, index)

    def field(name: String): JsonPath = JsonPath.Field(self, name)

    def index(index: Int): JsonPath = JsonPath.Index(self, index)
  object JsonPath:
    def identity: JsonPath = Identity

  /** REQUIREMENTS
    *
    *   1. Verify that a JSON path is a string, number, null, boolean, object, or array. 2. Verify
    *      that numbers are integer, non-zero, or within some range. 3. Verify that one part of a
    *      JSON value constraints another part. 4. Verify that a string can be interpreted as an ISO
    *      date time. 5. Verify that an object has a field. 6. Verify that an array has a certain
    *      minimum length. 7. Verify that a field in an object meets certain requirements. 8. Verify
    *      that an element in an array meets certain requirements. 9. Verify that all elements in an
    *      array meet certain requirements.
    */
  type Validation[+A]
  object Validation {}

  /** Implement the `validate` function that can validate some JSON and either return descriptive
    * error messages, or succeed with a unit value.
    */
  def validate[A](json: Json, validation: Validation[A]): Either[List[String], A] = ???
end input_validation

/** EXERCISE SET 5 - ETL
  *
  * Consider a domain where data must be loaded from various sources, processed through a flexible
  * graph of components.
  */
object data_processing:
  enum Schema:
    case Arbitrary
    case Null
    case Bool
    case Number
    case Str
    case Date
    case DateTime
    case Described(description: String, schema: Schema)
    case HasField(name: String, value: () => Schema)
    case Intersect(left: () => Schema, right: () => Schema)
    case Union(left: () => Schema, right: () => Schema)
    case Sequence(elementSchema: () => Schema)

    def self = this

    // Arbitrary & schema == schema & Arbitrary == schema
    def &(that: => Schema): Schema = Schema.Intersect(() => self, () => that)

    // Arbitrary | schema == schema | Arbitrary == schema
    def |(that: => Schema): Schema = Schema.Union(() => self, () => that)

    def ??(description: String): Schema = Schema.Described(description, self)
  end Schema
  object Schema:
    def hasField(name: String, value: => Schema): Schema = HasField(name, () => value)

    val number: Schema    = Number
    val string: Schema    = Str
    val bool: Schema      = Bool
    val arbitrary: Schema = Arbitrary
    val date: Schema      = Date
    val dateTime          = DateTime
    val nulls: Schema     = Null

    def list(schema: => Schema): Schema = Sequence(() => schema)

    lazy val personSchema: Schema =
      hasField("name", string) &
        hasField("age", number | nulls) &
        hasField("address", list(string)) &
        hasField("manager", personSchema | nulls)
  end Schema

  enum Data:
    case Null
    case Bool(value: Boolean)
    case Number(value: BigDecimal)
    case Str(value: String)
    case Date(value: java.time.LocalDate)
    case DateTime(value: java.time.LocalDateTime)
    case Record(value: Map[String, Data])
    case Sequence(value: List[Data])

    def schema: Schema =
      this match
        case Null            => Schema.Null
        case Bool(value)     => Schema.Bool
        case Number(value)   => Schema.Number
        case Str(value)      => Schema.Str
        case Date(value)     => Schema.Date
        case DateTime(value) => Schema.DateTime
        case Record(value)   =>
          value.map { case (name, data) => Schema.hasField(name, data.schema) }
            .reduceOption(_ & _) match
            case Some(value) => value
            case None        => Schema.arbitrary
        case Sequence(value) =>
          value.map(_.schema).toSet.reduceOption(_ | _) match
            case Some(value) => value
            case None        => Schema.arbitrary
  end Data

  /** EXERCISE 1
    *
    * Design a data type that can model schema transformations, as well as value transformations
    * (e.g. replacing nulls with values, replacing one type of value with another type.
    */
  enum Transformation:
    case Dummy
end data_processing

/** LOYALTY REWARDS PROGRAM - GRADUATION PROJECT
  *
  * Consider a software application that manages reward programs for businesses.
  */
object loyalty_program:
  import java.time.Instant

  enum Supplier:
    case Amex
    case UnitedAirlines

  enum LoyaltyCurrency:
    case Amex
    type Amex = Amex.type

  enum FiscalCurrency:
    case USD

  enum Numeric[A]:
    case IntNumeric    extends Numeric[Int]
    case DoubleNumeric extends Numeric[Double]

  given Numeric[Int]    = Numeric.IntNumeric
  given Numeric[Double] = Numeric.DoubleNumeric

  final case class Amount[Currency](value: BigDecimal, currency: Currency):
    def +(that: Amount[Currency]): Amount[Currency] =
      copy(value = value + that.value)
  object Amount:
    implicit def AmountNumeric[Currency]: Numeric[Amount[Currency]] = ???

  type FiscalAmount  = Amount[FiscalCurrency]
  type LoyaltyAmount = Amount[LoyaltyCurrency]

  final case class Portfolio(holdings: Map[LoyaltyCurrency, LoyaltyAmount]):
    def +(that: (LoyaltyCurrency, LoyaltyAmount)): Portfolio =
      copy(holdings =
        holdings.updated(that._1, holdings.get(that._1).map(_ + that._2).getOrElse(that._2))
      )

  /** Every loyalty program is issued by a supplier, has an associated currency, and has an ordered
    * list of tiers.
    */
  final case class LoyaltyProgram(
    defaultTier: Tier,
    tiers: List[Tier],
    supplier: Supplier,
    currency: LoyaltyCurrency
  )

  /** An account is created when a user enrolls in a loyalty program.
    */
  final case class Account(
    user: User,
    loyaltyProgram: LoyaltyProgram,
    tier: Tier,
    history: List[Activity]
  )

  final case class User(email: String)

  final case class EarnRate(loyaltyCurrency: LoyaltyCurrency, fiscalCurrency: FiscalCurrency)

  /** Tiers contain rule sets that define loyalty point rules for users who are currently in the
    * tier.
    */
  final case class Tier(
    benefits: Set[Benefit],
    name: String,
    description: String,
    legalese: String,
    ruleSet: RuleSet
  )

  final case class Benefit(description: String)

  enum IsLiteral[A]:
    case IntIsLiteral    extends IsLiteral[Int]
    case DoubleIsLiteral extends IsLiteral[Double]

  given IsLiteral[Int]    = IsLiteral.IntIsLiteral
  given IsLiteral[Double] = IsLiteral.DoubleIsLiteral

  /*
   * Rule sets represent sets of rules that can perform actions in response
   * to conditions being met. For example, if a user spends so much money,
   * then they may be eligible for an automatic tier promotion.
   */
  enum RuleSet:
    /** EXERCISE 1
      *
      * Augment `RuleSet` with a constructor that models execution of a `SystemAction` whenever a
      * `RuleCalculation[Boolean]` evaluates to true.
      */
    case When( /* ??? */ )

    def self = this

    /** EXERCISE 2
      *
      * Augment `RuleSet` with an operator that models combining two rule sets into one.
      */
    def &&(that: RuleSet): RuleSet = ???

    /** EXERCISE 3
      *
      * Augment `RuleSet` with an operator that models combining two rule sets into one.
      */
    def ||(that: RuleSet): RuleSet = ???
  end RuleSet

  enum RuleCalculation[+A]:
    case Widen[A, B](rc: RuleCalculation[A])(using A <:< B) extends RuleCalculation[B]

    def self = this

    /** EXERCISE 4
      *
      * Add an operator `&&` that applies only with this calculation and the other calculation
      * produce booleans, and which models the boolean conjunction ("and") of the two boolean
      * values.
      */
    def &&(that: RuleCalculation[Boolean])(implicit ev: A <:< Boolean): RuleCalculation[Boolean] =
      // This line of code "proves" that the "A" type is actually a Boolean:
      val self1: RuleCalculation[Boolean] = self.widen[Boolean]

      val _ = self1 // DELETE ME once you use `self1`
      ???

    /** EXERCISE 5
      *
      * Add an operator `||` that applies only with this calculation and the other calculation
      * produce booleans, and which models the boolean disjunction ("or") of the two boolean values.
      */
    def ||(that: RuleCalculation[Boolean])(implicit ev: A <:< Boolean): RuleCalculation[Boolean] =
      // This line of code "proves" that the "A" type is actually a Boolean:
      val self1: RuleCalculation[Boolean] = self.widen[Boolean]

      val _ = self1 // DELETE ME once you use `self1`

      ???

    /** EXERCISE 6
      *
      * Add an operator `negate` that applies only with this calculation produces a boolean, and
      * which models the boolean negation of this value.
      */
    def unary_!(implicit ev: A <:< Boolean): RuleCalculation[Boolean] =
      // This line of code "proves" that the "A" type is actually a Boolean:
      val self1: RuleCalculation[Boolean] = self.widen[Boolean]

      val _ = self1 // DELETE ME once you use `self1`

      ???

    /** EXERCISE 7
      *
      * Add an operator `>` that applies only when this calculation and the other calculation
      * produce amounts, and which models the `>` comparison between the two amounts, which yields a
      * boolean indicating if the relation holds.
      */
    def >[Currency: Numeric](that: RuleCalculation[Currency])(implicit
      ev: A <:< Currency
    ): RuleCalculation[Boolean] =
      // This line of code "proves" that the "A" type is actually a Currency:
      val self1: RuleCalculation[Currency] = self.widen[Currency]

      val _ = self1 // DELETE ME once you use `self1`

      ???

    /** EXERCISE 8
      *
      * Add an operator `>=` that applies only when this calculation and the other calculation
      * produce amounts, and which models the `>=` comparison between the two amounts, which yields
      * a boolean indicating if the relation holds.
      */
    def >=[Currency: Numeric](
      that: RuleCalculation[Currency]
    )(implicit ev: A <:< Currency): RuleCalculation[Boolean] =
      // This line of code "proves" that the "A" type is actually a Currency:
      val self1: RuleCalculation[Currency] = self.widen[Currency]

      val _ = self1 // DELETE ME once you use `self1`

      ???

    /** EXERCISE 9
      *
      * Add an operator `<` that applies only when this calculation and the other calculation
      * produce amounts, and which models the `<` comparison between the two amounts, which yields a
      * boolean indicating if the relation holds.
      */
    def <[Currency: Numeric](that: RuleCalculation[Currency])(implicit
      ev: A <:< Currency
    ): RuleCalculation[Boolean] =
      // This line of code "proves" that the "A" type is actually a Currency:
      val self1: RuleCalculation[Currency] = self.widen[Currency]

      val _ = self1 // DELETE ME once you use `self1`

      ???

    /** EXERCISE 10
      *
      * Add an operator `<=` that applies only when this calculation and the other calculation
      * produce amounts, and which models the `<=` comparison between the two amounts, which yields
      * a boolean indicating if the relation holds.
      */
    def <=[Currency: Numeric](
      that: RuleCalculation[Currency]
    )(implicit ev: A <:< Currency): RuleCalculation[Boolean] =
      // This line of code "proves" that the "A" type is actually a Currency:
      val self1: RuleCalculation[Currency] = self.widen[Currency]

      val _ = self1 // DELETE ME once you use `self1`

      ???

    /** EXERCISE 11
      *
      * Add an operator `===` that applies only when this calculation and the other calculation
      * produce amounts, and which models equality.
      */
    def ===[Currency: Numeric](
      that: RuleCalculation[Currency]
    )(implicit ev: A <:< Currency): RuleCalculation[Boolean] =
      // This line of code "proves" that the "A" type is actually a Currency:
      val self1: RuleCalculation[Currency] = self.widen[Currency]

      val _ = self1 // DELETE ME once you use `self1`

      ???

    def widen[B](using A <:< B): RuleCalculation[B] = RuleCalculation.Widen(self)
  end RuleCalculation
  object RuleCalculation:

    /** EXERCISE 12
      *
      * Add a constructor that models calculation of a constant value of the specified type.
      */
    def constant[A](value: A): RuleCalculation[A] = ???

    /** EXERCISE 13
      *
      * Add a constructor that models calculation of the price of an item that the user buys, in a
      * fiscal currency.
      */
    def purchasePrice: RuleCalculation[FiscalCurrency] = ???

    /** EXERCISE 14
      *
      * Add a constructor that models calculation of the price of an item that the user buys, in a
      * fiscal currency.
      */
    def itemPrice: RuleCalculation[FiscalCurrency] = ???

    /** EXERCISE 15
      *
      * Add a constructor that models the number of days since the last purchase of the user, as an
      * integer.
      */
    def daysSinceLastPurchase: RuleCalculation[Int] = ???
  end RuleCalculation

  enum UserAction:
    case Spend(amount: FiscalCurrency)
    case Return(amount: FiscalCurrency)

  enum SystemAction:
    case Credit(amount: LoyaltyCurrency)
    case Debit(amount: LoyaltyCurrency)
    case TierPromotion
    case TierDemotion
    case ChangeStatus(status: UserProgramStatus)

  final case class Activity(action: Either[UserAction, SystemAction], instant: Instant)

  enum UserProgramStatus:
    case Active
    case Inactive
    case Suspended(who: User, why: String)

  /** EXERCISE 14
    *
    * Construct a rule set that describes promotion to the next tier, as well as demotion, and
    * changing the status of the user to inactive.
    */
  def ruleSet: RuleSet = ???

  /** Example of running a rule set on the history of a user to produce system actions.
    */
  def run(history: List[UserAction], ruleSet: RuleSet): List[SystemAction] = ???

  /** Example of describing a rule set in a human-readable form.
    */
  def describe(ruleSet: RuleSet): String = ???
end loyalty_program
