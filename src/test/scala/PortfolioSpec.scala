package mwt.krypton

import org.scalatest.{FlatSpec, Matchers}
import util.Success

class PortfolioSpec extends FlatSpec with Matchers {
  import Protocol.Position._
  import Protocol.Portfolio._
  import Model.PDecimal
  import Model.ISIN
  import Update._

  behavior of "PDecimal" 

  it should "raise exception when instantiated with negative number" in {
    assertThrows[IllegalArgumentException] { // Result type: Assertion
      PDecimal(-1.0f)
    }
  }

  behavior of "Portfolio"

  def genPosition(isin: String, quantity: BigDecimal,
    priceBuy: BigDecimal, lastPrice: BigDecimal) = 
      Model.Position(ISIN(isin), PDecimal(quantity), 
        PDecimal(priceBuy),PDecimal(lastPrice))

  it should "be empty when initialized" in {
    Model.Portfolio().positions.size shouldEqual 0
  }

  it should "replace Position" in {
    val portfolio = Model.Portfolio()
    val isin = ISIN("DE01")
    val p = genPosition("DE01",1.0, 1.0, 1.0) 
    
    val updated = update(portfolio, PortfolioTransport(ReplacePosition(isin, p)))
    updated.isSuccess shouldEqual true
    updated.get.positions.size shouldEqual 1
  }

  it should "replace position with 2 positions" in {
    val portfolio = Model.Portfolio()
    val isin = ISIN("DE01")
    val isin2 = ISIN("DE02")
    val p = genPosition("DE01",1.0, 1.0, 1.0) 
    val p2 = genPosition("DE02",2.0, 2.0, 2.0)

    val end = for { 
      a <- update(portfolio, PortfolioTransport(ReplacePosition(isin, p)))
      b <- update(a, PortfolioTransport(ReplacePosition(isin2, p2)))
    } yield b

    end.isSuccess shouldEqual true
    end.get.positions.size shouldEqual 2
  }

  it should "delete position" in {
    val portfolio = Model.Portfolio()
    val isin = ISIN("DE01")
    val p = genPosition("DE01",1.0, 1.0, 1.0) 
    
    val end = for { 
      newS <- update(portfolio, PortfolioTransport(ReplacePosition(isin, p)))
      end <- update(newS, PortfolioTransport(DeletePosition(isin)))
    } yield end
    end.isSuccess shouldEqual true
  }

  it should "fail when deleting not existent Position" in {
    val portfolio = Model.Portfolio()
    val result = update(portfolio, PortfolioTransport(DeletePosition(ISIN("x"))))
    result.isSuccess shouldEqual false
  }

  behavior of "Position"

  it should "update quantity, last price and buy price for position" in {
    val portfolio = Model.Portfolio()
    val isin = ISIN("DE01")
    val p = genPosition("DE01",0.0, 0.0, 0.0) 
    
    val end = for { 
      newS <- update(portfolio, PortfolioTransport(ReplacePosition(isin, p)))
      a <- update(newS, PositionTransport(isin, ChangeQuantity(PDecimal(1.0)))) 
      _ <- Success(a.positions.get(isin).get shouldEqual genPosition("DE01", 1.0, 0.0, 0.0))
      b <- update(a, PositionTransport(isin, ChangeLastPrice(PDecimal(1.0)))) 
      _ <- Success(b.positions.get(isin).get shouldEqual genPosition("DE01", 1.0, 0.0, 1.0))
      c <- update(b, PositionTransport(isin, ChangePriceBuy(PDecimal(1.0)))) 
      _ <- Success(c.positions.get(isin).get shouldEqual genPosition("DE01", 1.0, 1.0, 1.0))
    } yield c 
    
    println(end)
    end.isSuccess shouldEqual true
  }

}
