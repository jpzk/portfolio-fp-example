package mwt.krypton

import math.BigDecimal
import collection.immutable.Map
import util.{Try, Success, Failure}

/**
 * Design remarks:
 *
 * - Purely functional, no side-effects
 *   -- Data and logic seperation 
 *   -- No glitches possible due to side-effects (very important for banking) 
 *   -- Better testing, decomposition of update functions
 *   -- Better traceability of updates (e.g. using command objects)
 *
 * - Using the Try monad to propagate exceptions (due to validation etc.) 
 *   to the state transitions.
 *
 * - Instead of BigDecimal, I use PDecimal which only allows positive numbers.
 *   -- It validates against negative numbers as early as possible in a type-safe way.
 *
 * - Added validation functions for showing how validation using more conditions
 *   could be implemented.
 *
 * For questions: jendrik@madewithtea.com 
 */
object Model {
  case class ISIN(isin: String)

  case class PDecimal(d: BigDecimal) {
    require(d >= 0.0)
  }

  case class Position(isin: ISIN, 
    quantity: PDecimal, 
    priceBuy: PDecimal, 
    lastPrice: PDecimal)  
  
  case class Portfolio(positions: Map[ISIN, Position] = Map()) {
    def getPosition(isin: ISIN): Option[Position] = positions.get(isin)
  }
}

object Protocol {
  import Model._

  sealed class Transport

  object Position { 
    case class PositionTransport[T <: Update](isin: ISIN, update: T) extends Transport
    
    sealed class Update 
    case class ChangeQuantity(quantity: PDecimal) extends Update 
    case class ChangePriceBuy(priceBuy: PDecimal) extends Update
    case class ChangeLastPrice(lastPrice: PDecimal) extends Update
  }

  object Portfolio { 
    case class PortfolioTransport[T <: Update](update: T) extends Transport

    sealed class Update
    case class ReplacePosition(isin: ISIN, position: Position) extends Update 
    case class DeletePosition(isin: ISIN) extends Update 

    case class PositionDoesNotExist(p: ISIN) 
      extends Exception(s"Position for $p does not exist")
  }

  object Errors {
    case class UnknownPortfolioUpdate(p: Portfolio) 
      extends Exception(s"Unknown update on portfolio $p")

    case class UnknownPositionUpdate(p: Position) 
      extends Exception(s"Unknown update on position $p")

    case class UnknownUpdate() 
     extends Exception(s"Unknown update")
  }
}

object Update { 
  import Model._
  import Protocol._
  import Protocol.Errors._
  import Protocol.Position._
  import Protocol.Portfolio._
  import Protocol.Position.{Update => PositionUpdate}
  import Protocol.Portfolio.{Update => PortfolioUpdate}

  object Position {

    // could be u@ChangeQuantity(n) only Success if there are sufficient funds
    def validate(position: Position, update: PositionUpdate): Try[PositionUpdate] = 
      update match {
        case u@ChangeQuantity(n) => Success(u) 
        case u@ChangePriceBuy(n) => Success(u)
        case u@ChangeLastPrice(n) => Success(u)
        case _ => Failure(UnknownPositionUpdate(position))
      }

    def update(position: Position, update: PositionUpdate): Try[Position] = 
      validate(position, update).flatMap { validatedUpdate => 
        validatedUpdate match {
          case ChangeQuantity(n) => Success(position.copy(quantity = n))
          case ChangePriceBuy(d) => Success(position.copy(priceBuy = d))
          case ChangeLastPrice(d) => Success(position.copy(lastPrice = d))
          case _ => Failure(UnknownPositionUpdate(position))
        }
      }
  }

  object Portfolio { 

    def hasISIN(portfolio: Portfolio, isin: ISIN) = portfolio.positions contains isin

    def validate(portfolio: Portfolio, update: PortfolioUpdate): Try[PortfolioUpdate] = 
      update match {
        case u@ReplacePosition(isin, p) => Success(u) 
        case u@DeletePosition(isin) => if(hasISIN(portfolio, isin))
            Success(u) 
          else Failure(PositionDoesNotExist(isin))

        case _ => Failure(UnknownPortfolioUpdate(portfolio))
      }

    def update(state: Portfolio, update: PortfolioUpdate): Try[Portfolio] = 
      validate(state, update).flatMap { validatedUpdate => 
        update match { 
          case ReplacePosition(isin, p) => 
            val newPositions = state.positions + (isin -> p)
            Success(state.copy(positions = newPositions))

          case DeletePosition(isin) => 
            val newPositions = state.positions - isin
            Success(state.copy(positions = newPositions))

          case _ => Failure(UnknownPortfolioUpdate(state))
        }
      }
  }

  def update(state: Portfolio, transport: Transport): Try[Portfolio] = 
    transport match {
      case PositionTransport(isin, update) => 
        val position = state.getPosition(isin) match {
          case Some(p) => Success(p)
          case None => Failure(PositionDoesNotExist(isin))
        }
        position
          .flatMap { p => Position.update(p, update) } 
          .flatMap { updated => 
            val newPositions = state.positions + (isin -> updated)
            Success(state.copy(positions = newPositions)) 
          } 

      case PortfolioTransport(update) => Portfolio.update(state, update)
      case _ => Failure(UnknownUpdate()) 
    }
}


