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
     * Augment `RuleSet` with a constructor that models execution of a `RuleAction`
     * whenever a `RuleCalculation[Boolean]` evaluates to true.
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
      ???

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
  }
  object RuleCalculation {

    /**
     * EXERCISE 11
     *
     * Add a constructor that models calculation of a constant value of the
     * specified type.
     */
    final case class Constant[A]()

    /**
     * EXERCISE 12
     *
     * Add a constructor that models calculation of the price of an item that
     * the user buys, in a fiscal currency.
     */
    final case class PurchasePrice()

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

  sealed trait RuleAction
  object RuleAction {
    sealed trait UserAction extends RuleAction
    object UserAction {
      final case class Spend(amount: FiscalCurrency)  extends UserAction
      final case class Return(amount: FiscalCurrency) extends UserAction
    }

    sealed trait SystemAction extends RuleAction
    object SystemAction {
      final case class Credit(amount: LoyaltyCurrency)         extends SystemAction
      final case class Debit(amount: LoyaltyCurrency)          extends SystemAction
      case object TierPromotion                                extends SystemAction
      case object TierDemotion                                 extends SystemAction
      final case class ChangeStatus(status: UserProgramStatus) extends SystemAction
    }
  }

  final case class Activity(action: RuleAction, instant: Instant)

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

  /**
   * EXERCISE 1
   *
   * Explore the structure of `CalendarStatus` by deciding what composable,
   * orthogonal operations to add to the data type.
   */
  sealed trait CalendarStatus
  object CalendarStatus {
    case object Busy extends CalendarStatus
    case object Free extends CalendarStatus
  }

  /**
   * EXERCISE 2
   *
   * Explore the structure of `CalendarRegion` by deciding what composable,
   * orthogonal operations to add to the data type.
   */
  final case class CalendarRegion(span: TimeSpan, status: CalendarStatus)

  /**
   * EXERCISE 3
   *
   * Explore the structure of `DailySchedule` by deciding what composable,
   * orthogonal operations to add to the data type.
   *
   * HINT: Consider the union and intersection of two daily schedules.
   */
  final case class DailySchedule(set: Set[CalendarRegion])

  /**
   * EXERCISE 4
   *
   * Explore the structure of `MonthlySchedule` by deciding what composable,
   * orthogonal operations to add to the data type.
   */
  final case class MonthlySchedule(daysOfMonth: Vector[DailySchedule])

  final case class Person(name: String)

  /**
   * EXERCISE 5
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
  }
}
