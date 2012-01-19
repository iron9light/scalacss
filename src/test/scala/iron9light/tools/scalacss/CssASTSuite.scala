package iron9light.tools.scalacss

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import CssAST._

/**
 * @author il
 * @version 11/15/11 11:35 PM
 */

@RunWith(classOf[JUnitRunner])
class CssASTSuite extends FunSuite {
  test("render") {
    val cssObject = CssRule(SelectorElement("div") :: SelectorElement("a") :: Pseudo("::secetion") :: Nil,
      Declaration(CssProperty("color"), CssValue("#000000") :: Nil) :: Declaration(CssProperty("background-color"), CssValue("#FFFFFF") :: Nil) :: Nil)
    val doc = render(cssObject :: Nil)
    println(Printer.pretty(doc))
    println()
    println(Printer.compact(doc))
  }
}