package net.degoes

/*
 * INTRODUCTION
 *
 * In Functional Design, the most powerful approaches utilize GADTs to model
 * solutions to the domain problem, exposing a minimal set of constructors
 * and operators that have the full level of power required by the domain,
 * but no more; and which are both minimal and orthogonal.
 *
 * In this section, you'll tie together everything you've learned to develop
 * hands-on skills in applied functional design.
 */

/**
 * LOYALTY REWARDS PROGRAM - EXERCISE SET 1
 *
 * Consider a software application that manages reward programs for businesses.
 */
object loyalty_program {
  import java.time.Instant

  sealed trait Supplier
  object Supplier {
    case object Amex           extends Supplier
    case object UnitedAirlines extends Supplier
  }

  sealed trait LoyaltyCurrency
  object LoyaltyCurrency {
    case object Amex extends LoyaltyCurrency
    type Amex = Amex.type
  }

  sealed trait FiscalCurrency
  object FiscalCurrency {
    final case object USD extends FiscalCurrency
  }

  final case class Amount[Currency](value: BigDecimal, currency: Currency) {
    def +(that: Amount[Currency]): Amount[Currency] =
      copy(value = value + that.value)
  }
  object Amount {
    implicit def AmountNumeric[Currency]: Numeric[Amount[Currency]] = ???
  }

  type FiscalAmount  = Amount[FiscalCurrency]
  type LoyaltyAmount = Amount[LoyaltyCurrency]

  final case class Portfolio(holdings: Map[LoyaltyCurrency, LoyaltyAmount]) {
    def +(that: (LoyaltyCurrency, LoyaltyAmount)): Portfolio =
      copy(holdings = holdings.updated(that._1, holdings.get(that._1).map(_ + that._2).getOrElse(that._2)))
  }

  /**
   * Every loyalty program is issued by a supplier, has an associated currency,
   * and has an ordered list of tiers.
   */
  final case class LoyaltyProgram(defaultTier: Tier, tiers: List[Tier], supplier: Supplier, currency: LoyaltyCurrency)

  /**
   * An account is created when a user enrolls in a loyalty program.
   */
  final case class Account(user: User, loyaltyProgram: LoyaltyProgram, tier: Tier, history: List[Activity])

  final case class User(email: String)

  final case class EarnRate(loyaltyCurrency: LoyaltyCurrency, fiscalCurrency: FiscalCurrency)

  /**
   * Tiers contain rule sets that define loyalty point rules for users who are
   * currently in the tier.
   */
  final case class Tier(benefits: Set[Benefit], name: String, description: String, legalese: String, ruleSet: RuleSet)

  final case class Benefit(description: String)

  /*
   * Rule sets represent sets of rules that can perform actions in response
   * to conditions being met. For example, if a user spends so much money,
   * then they may be eligible for an automatic tier promotion.
   */
  sealed trait RuleSet {

    /**
     * EXERCISE 1
     *
     * Augment `RuleSet` with an operator that models combining two rule sets
     * into one, applying the actions of both.
     */
    def &&(that: RuleSet): RuleSet = ???

    /**
     * EXERCISE 2
     *
     * Augment `RuleSet` with an operator that models combining two rule sets
     * into one, applying either the left (if it results in an action) or the
     * right (if the left does not result in an action)
     */
    def ||(that: RuleSet): RuleSet = ???
  }
  object RuleSet {

    /**
     * EXERCISE 3
     *
     * Augment `RuleSet` with a constructor that models execution of a
     * `SystemAction` whenever a `RuleCalculation[Boolean]` evaluates to
     * true.
     */
    final case class When() extends RuleSet
  }

  sealed trait RuleCalculation[+A] { self =>

    /**
     * EXERCISE 4
     *
     * Add an operator `&&` that applies only with this calculation and the other
     * calculation produce booleans, and which models the boolean conjunction
     * ("and") of the two boolean values.
     */
    def &&(that: RuleCalculation[Boolean])(implicit ev: A <:< Boolean): RuleCalculation[Boolean] = ???

    /**
     * EXERCISE 5
     *
     * Add an operator `||` that applies only with this calculation and the other
     * calculation produce booleans, and which models the boolean disjunction
     * ("or") of the two boolean values.
     */
    def ||(that: RuleCalculation[Boolean])(implicit ev: A <:< Boolean): RuleCalculation[Boolean] = ???

    /**
     * EXERCISE 6
     *
     * Add an operator `negate` that applies only with this calculation produces
     * a boolean, and which models the boolean negation of this value.
     */
    def negate(implicit ev: A <:< Boolean): RuleCalculation[Boolean] = ???

    /**
     * EXERCISE 7
     *
     * Add an operator `>` that applies only when this calculation and the
     * other calculation produce amounts, and which models the `>` comparison
     * between the two amounts, which yields a boolean indicating if the
     * relation holds.
     */
    def >[Currency: Numeric](that: RuleCalculation[Currency])(implicit ev: A <:< Currency): RuleCalculation[Boolean] =
      RuleCalculation.GreaterThan(self.widen[Currency](ev), that)

    /**
     * EXERCISE 8
     *
     * Add an operator `>=` that applies only when this calculation and the
     * other calculation produce amounts, and which models the `>=` comparison
     * between the two amounts, which yields a boolean indicating if the
     * relation holds.
     */
    def >=[Currency: Numeric](that: RuleCalculation[Currency])(implicit ev: A <:< Currency): RuleCalculation[Boolean] =
      ???

    /**
     * EXERCISE 9
     *
     * Add an operator `<` that applies only when this calculation and the
     * other calculation produce amounts, and which models the `<` comparison
     * between the two amounts, which yields a boolean indicating if the
     * relation holds.
     */
    def <[Currency: Numeric](that: RuleCalculation[Currency])(implicit ev: A <:< Currency): RuleCalculation[Boolean] =
      ???

    /**
     * EXERCISE 10
     *
     * Add an operator `<=` that applies only when this calculation and the
     * other calculation produce amounts, and which models the `<=` comparison
     * between the two amounts, which yields a boolean indicating if the
     * relation holds.
     */
    def <=[Currency: Numeric](that: RuleCalculation[Currency])(implicit ev: A <:< Currency): RuleCalculation[Boolean] =
      ???

    def widen[B](implicit ev: A <:< B): RuleCalculation[B] = RuleCalculation.Widen(self)(ev)
  }
  object RuleCalculation {
    final case class GreaterThan[A: Numeric](left: RuleCalculation[A], right: RuleCalculation[A])
        extends RuleCalculation[Boolean]
    final case class Widen[A, B](rc: RuleCalculation[A])(implicit val ev: A <:< B) extends RuleCalculation[B]

    /**
     * EXERCISE 11
     *
     * Add a constructor that models calculation of a constant value of the
     * specified type.
     */
    final case class Constant[A](value: A) extends RuleCalculation[A]

    /**
     * EXERCISE 12
     *
     * Add a constructor that models calculation of the price of an item that
     * the user buys, in a fiscal currency.
     */
    final case class PurchasePrice() extends RuleCalculation[FiscalAmount]

    /**
     * EXERCISE 13
     *
     * Add a constructor that models calculation of the price of an item that
     * the user buys, in a fiscal currency.
     */
    final case class ItemPrice()

    /**
     * EXERCISE 14
     *
     * Add a constructor that models the number of days since the last purchase
     * of the user, as an integer.
     */
    final case class DaysSinceLastPurchase()
  }

  sealed trait UserAction
  object UserAction {
    final case class Spend(amount: FiscalCurrency)  extends UserAction
    final case class Return(amount: FiscalCurrency) extends UserAction
  }

  sealed trait SystemAction
  object SystemAction {
    final case class Credit(amount: LoyaltyCurrency)         extends SystemAction
    final case class Debit(amount: LoyaltyCurrency)          extends SystemAction
    case object TierPromotion                                extends SystemAction
    case object TierDemotion                                 extends SystemAction
    final case class ChangeStatus(status: UserProgramStatus) extends SystemAction
  }

  final case class Activity(action: Either[UserAction, SystemAction], instant: Instant)

  sealed trait UserProgramStatus
  object UserProgramStatus {
    case object Active                                 extends UserProgramStatus
    case object Inactive                               extends UserProgramStatus
    final case class Suspended(who: User, why: String) extends UserProgramStatus
  }

  /**
   * EXERCISE 15
   *
   * Construct a rule set that describes promotion to the next tier, as
   * well as demotion, and changing the status of the user to inactive.
   */
  def ruleSet: RuleSet = ???

  def run(history: List[UserAction], ruleSet: RuleSet): List[SystemAction] = ???

  def describe(ruleSet: RuleSet): String = ???
}

/**
 * CALENDER SCHEDULING APP - EXERCISE SET 2
 */
object calendar {
  final case class HourOfDay(value: Int) {
    def to(that: HourOfDay): Stream[HourOfDay] = (value to that.value).toStream.map(HourOfDay(_))

    def until(that: HourOfDay): Stream[HourOfDay] = (value until that.value).toStream.map(HourOfDay(_))
  }
  object HourOfDay {
    val min = HourOfDay(0)
    val max = HourOfDay(24)
  }

  sealed trait DayOfWeek
  object DayOfWeek {
    case object Sunday    extends DayOfWeek
    case object Monday    extends DayOfWeek
    case object Tuesday   extends DayOfWeek
    case object Wednesday extends DayOfWeek
    case object Thursday  extends DayOfWeek
    case object Friday    extends DayOfWeek
    case object Saturday  extends DayOfWeek
  }

  final case class TimeSpan(start: HourOfDay, end: HourOfDay)
  object TimeSpan {
    val empty: TimeSpan = TimeSpan(HourOfDay(0), HourOfDay(0))
  }

  /**
   * EXERCISE 1
   *
   * Explore the structure of `CalendarRegion` by deciding what composable,
   * orthogonal operations to add to the data type.
   */
  final case class CalendarAppointment(span: TimeSpan) { self =>
  }
  object CalendarAppointment {
    val empty: CalendarAppointment = CalendarAppointment(TimeSpan.empty)
  }

  /**
   * EXERCISE 2
   *
   * Explore the structure of `DailySchedule` by deciding what composable,
   * orthogonal operations to add to the data type.
   *
   * HINT: Consider the union, intersection, & complement of two daily schedules.
   */
  final case class DailySchedule(set: Set[CalendarAppointment]) { self =>
  }
  object DailySchedule {
    val empty: DailySchedule = DailySchedule(Set())
  }

  /**
   * EXERCISE 3
   *
   * Explore the structure of `MonthlySchedule` by deciding what composable,
   * orthogonal operations to add to the data type.
   */
  final case class MonthlySchedule(daysOfMonth: Vector[DailySchedule]) {}
  object MonthlySchedule {
    val empty: MonthlySchedule = MonthlySchedule(Vector())
  }

  final case class Person(name: String)

  /**
   * EXERCISE 4
   *
   * Using the operators you build, express a solution to the following
   * problem: find all the free times that a group of friends can virtually
   * meet for the specified number of hours.
   */
  def findFreeTimes(lengthInHours: Int, friends: Map[Person, MonthlySchedule]): MonthlySchedule = ???

}

/**
 * CMS - EXERCISE SET 3
 *
 * Consider a content-management system.
 */
object cms {

  /**
   * EXERCISE 1
   *
   * Add whatever transformations and combinations have well-defined semantics.
   */
  sealed trait Html { self =>
    final def isEmpty: Boolean = self match {
      case Html.Zero                => true
      case Html.One(_, _, children) => false
      case Html.Many(elements)      => elements.forall(_.isEmpty)
    }

    final def childList: List[Html] = self match {
      case Html.Zero                => Nil
      case Html.One(_, _, children) => children.childList
      case Html.Many(elements)      => elements.flatMap(_.childList)
    }
  }
  object Html {
    case object Zero                                                                       extends Html
    final case class One(tagName: String, attributes: Map[String, String], children: Html) extends Html
    final case class Many(elements: List[Html])                                            extends Html
  }

  final case class User(email: String)

  trait PageContext {
    def url: java.net.URL
  }
  trait UserContext {
    def user: User
  }

  /**
   * EXERCISE 2
   *
   * Add whatever transformations and combinations have well-defined semantics.
   *
   */
  sealed trait Component[-Context, -State] {
    def render(context: Context, state: State): Html
  }
  object Component {
    final case class Leaf[Context, State](render0: (Context, State) => Html) extends Component[Context, State] {
      def render(context: Context, state: State): Html = render0(context, state)
    }

    def make[Context, State](render: (Context, State) => Html): Component[Context, State] = Leaf(render)

    /**
     * EXERCISE 3
     *
     * Add some example components.
     */
    val components = ???
  }
}

/**
 * JSON VALIDATION - EXERCISE SET 4
 *
 * Consider a domain where incoming JSON documents are stored into a NoSQL
 * database, but before being stored, they must be validated by flexible
 * rules. If validation fails, descriptive error messages must be generated
 * that allow clients of the JSON endpoint to fix the issues with their data.
 */
object input_validation {
  sealed trait Json {

    /**
     * EXERCISE 1
     *
     * Implement a method to retrieve the JSON value at the specified path, or
     * fail with a descriptive error message.
     */
    def get(path: JsonPath): Either[String, Json] = ???
  }
  object Json {
    case object Null                                  extends Json
    final case class Bool(value: Boolean)             extends Json
    final case class Number(value: BigDecimal)        extends Json
    final case class Text(value: String)              extends Json
    final case class Sequence(value: List[Json])      extends Json
    final case class Object(value: Map[String, Json]) extends Json
  }

  sealed trait JsonPath { self =>
    def +(that: JsonPath): JsonPath =
      that match {
        case JsonPath.Identity             => self
        case JsonPath.Field(parent, name)  => JsonPath.Field(self + parent, name)
        case JsonPath.Index(parent, index) => JsonPath.Index(self + parent, index)
      }

    def field(name: String): JsonPath = JsonPath.Field(self, name)

    def index(index: Int): JsonPath = JsonPath.Index(self, index)
  }
  object JsonPath {
    case object Identity                                   extends JsonPath
    final case class Field(parent: JsonPath, name: String) extends JsonPath
    final case class Index(parent: JsonPath, index: Int)   extends JsonPath

    def identity: JsonPath = Identity
  }

  /**
   * REQUIREMENTS
   *
   * 1. Verify that a JSON path is a string, number, null, boolean, object, or array.
   * 2. Verify that numbers are integer, non-zero, or within some range.
   * 3. Verify that one part of a JSON value constraints another part.
   * 4. Verify that a string can be interpreted as an ISO date time.
   * 5. Verify that an object has a field.
   * 6. Verify that an array has a certain minimum length.
   * 7. Verify that a field in an object meets certain requirements.
   * 8. Verify that an element in an array meets certain requirements.
   * 9. Verify that all elements in an array meet certain requirements.
   */
  type Validation[+A]
  object Validation {}

  /**
   * Implement the `validate` function that can validate some JSON and either
   * return descriptive error messages, or succeed with a unit value.
   */
  def validate[A](json: Json, validation: Validation[A]): Either[List[String], A] = ???
}

/**
 * DATA PROCESSING - EXERCISE SET 5
 *
 * Consider a domain where data must be loaded from various sources, processed
 * through a flexible graph of components.
 */
object data_processing {
  import zio._

  type Unknown

  sealed trait Expr[-A, +B]
  object Expr {}

  sealed trait SourceDefinition
  object SourceDefinition {}

  def load(source: SourceDefinition): Source[Unknown] = ???

  sealed trait Flow[-Input, +Output]

  type Transformer[-A, +B] = Flow[A, B]
  type Source[+A]          = Flow[Unit, A]
  type Sink[-A]            = Flow[A, Unit]
  type Pipeline            = Flow[Unit, Unit]

  def execute(pipeline: Pipeline): Task[Unit] = ???

}
