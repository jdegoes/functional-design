package net.degoes

import java.time.Instant
import java.net.URL

/*
 * INTRODUCTION
 *
 * Functional Design depends heavily on functional data modeling. Functional
 * data modeling is the task of creating precise, type-safe models of a given
 * domain using algebraic data types and generalized algebraic data types.
 *
 * In this section, you'll review basic functional domain modeling.
 */

/** E-COMMERCE - EXERCISE SET 1
  *
  * Consider an e-commerce application that allows users to purchase products.
  */
object credit_card:

  /** EXERCISE 1
    *
    * Using only enums and case classes, create an immutable data model of a credit card, which must
    * have:
    *
    * * Number * Name * Expiration date * Security code
    */
  final case class CreditCard(
    number: CreditCardNumber, 
    name: Name, 
    expirationDate: java.time.YearMonth, 
    securityCode: SecurityCode)

  final case class Name(value: RangedVector[NameChar, 1, 255])

  enum NameChar:
    case A 
    case B 

  final case class CreditCardNumber(value: RangedVector[Digit, 12, 16])

  final case class SecurityCode(value: RangedVector[Digit, 3, 4])

  trait RangedVector[A, Min <: Int, Max <: Int]

  enum Digit: 
    case Zero extends Digit
    case One extends Digit
    case Two extends Digit
    case Three extends Digit
    case Four extends Digit
    case Five extends Digit
    case Six extends Digit
    case Seven extends Digit
    case Eight extends Digit
    case Nine extends Digit

  enum CreditCardType:
    case Visa
    case Mastercard
    case Amex
    case Discover

  enum Amount:
    case USD(value: BigDecimal)
    case EUR(value: BigDecimal)
    case GBP(value: BigDecimal)

  Amount.USD(BigDecimal(1))

  /** EXERCISE 2
    *
    * Using only enums and case classes, create an immutable data model of a product, which could be
    * a physical product, such as a gallon of milk, or a digital product, such as a book or movie,
    * or access to an event, such as a music concert or film showing.
    */
  enum Good:
    case Physical(name: String, price: Amount, weight: Weight, dimensions: Dimensions)
    case Digital(name: String, price: Amount)
    case Event(name: String, price: Amount, start: java.time.LocalDate, end: java.time.LocalDate, location: Location)

  final case class Weight()
  final case class Dimensions()
  final case class Address()

  enum Location:
    case Online(url: URL)
    case Physical(address: Address)

  /** EXERCISE 3
    *
    * Using only enums and case classes, create an immutable data model of a product price, which
    * could be one-time purchase fee, or a recurring fee on some regular interval.
    */
  enum PricingSchema:
    case OneTime(value: Amount)
    case Recurring(value: Amount, interval: java.time.Period)
end credit_card

/** EVENT PROCESSING - EXERCISE SET 3
  *
  * Consider an event processing application, which processes events from both devices, as well as
  * users.
  */
object events:

  /** EXERCISE
    *
    * Refactor the object-oriented data model in this section to a more functional one, which uses
    * only enums and case classes.
    */
  final case class Event(id: Int, time: Instant, payload: Payload)

  enum Payload:
    case User(userName: String, payload: UserPayload)
    case Device(deviceId: Int, payload: DevicePayload)

  enum UserPayload:
    case Purchase(item: String, price: Double)
    case AccountCreated

  enum DevicePayload:
    case SensorUpdated(reading: Option[Double])
    case DeviceActivated

  enum Event2:
    case UserPurchase(id: Int, time: Instant, userName: String, item: String, price: Double)
    case UserAccountCreated(id: Int, time: Instant, userName: String)
    case DeviceSensorUpdated(id: Int, time: Instant, deviceId: Int, reading: Option[Double])
    case DeviceActivated(id: Int, time: Instant, deviceId: Int)

    def id: Int 
    def time: Instant
end events

/** DOCUMENT EDITING - EXERCISE SET 4
  *
  * Consider a web application that allows users to edit and store documents of some type (which is
  * not relevant for these exercises).
  */
object documents:
  final case class UserId(identifier: String)
  final case class DocId(identifier: String)
  final case class DocContent(body: String)

  /** EXERCISE 1
    *
    * Using only enums and case classes, create a simplified but somewhat realistic model of a
    * Document.
    */
  case class Document(owner: UserId, id: DocId, content: DocContent)

  /** EXERCISE 2
    *
    * Using only enums and case classes, create a model of the access type that a given user might
    * have with respect to a document. For example, some users might have read-only permission on a
    * document.
    */
  enum AccessType:
    case None
    case ReadOnly
    case ReadWrite
    case Admin

  /** EXERCISE 3
    *
    * Using only enums and case classes, create a model of the permissions that a user has on a set
    * of documents they have access to. Do not store the document contents themselves in this model.
    */
  case class DocPermissions(lookup: Map[DocId, AccessType]):
    def apply(docId: DocId): AccessType = lookup.getOrElse(docId, AccessType.None)
end documents

/** BANKING - EXERCISE SET 5
  *
  * Consider a banking application that allows users to hold and transfer money.
  */
object bank:

  /** EXERCISE 1
    *
    * Using only enums and case classes, develop a model of a customer at a bank.
    */
  type Customer

  /** EXERCISE 2
    *
    * Using only enums and case classes, develop a model of an account type. For example, one
    * account type allows the user to write checks against a given currency. Another account type
    * allows the user to earn interest at a given rate for the holdings in a given currency.
    */
  type AccountType

  /** EXERCISE 3
    *
    * Using only enums and case classes, develop a model of a bank account, including details on the
    * type of bank account, holdings, customer who owns the bank account, and customers who have
    * access to the bank account.
    */
  type Account
end bank

/** STOCK PORTFOLIO - GRADUATION PROJECT
  *
  * Consider a web application that allows users to manage their portfolio of investments.
  */
object portfolio:

  /** EXERCISE 1
    *
    * Using only enums and case classes, develop a model of a stock exchange. Ensure there exist
    * values for NASDAQ and NYSE.
    */
  type Exchange

  /** EXERCISE 2
    *
    * Using only enums and case classes, develop a model of a currency type.
    */
  type CurrencyType

  /** EXERCISE 3
    *
    * Using only enums and case classes, develop a model of a stock symbol. Ensure there exists a
    * value for Apple's stock (APPL).
    */
  type StockSymbol

  /** EXERCISE 4
    *
    * Using only enums and case classes, develop a model of a portfolio held by a user of the web
    * application.
    */
  type Portfolio

  /** EXERCISE 5
    *
    * Using only enums and case classes, develop a model of a user of the web application.
    */
  type User

  /** EXERCISE 6
    *
    * Using only enums and case classes, develop a model of a trade type. Example trade types might
    * include Buy and Sell.
    */
  type TradeType

  /** EXERCISE 7
    *
    * Using only enums and case classes, develop a model of a trade, which involves a particular
    * trade type of a specific stock symbol at specific prices.
    */
  type Trade
end portfolio
