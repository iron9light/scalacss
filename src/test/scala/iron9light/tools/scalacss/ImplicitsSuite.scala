package iron9light.tools.scalacss

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import CssAST._

/**
 * @author il
 * @version 11/16/11 6:04 PM
 */

@RunWith(classOf[JUnitRunner])
class ImplicitsSuite extends Implicits with FunSuite with ShouldMatchers {
  test("toSelector") {
    val plus: Selector = "+"
    plus should be === Combinator("+")

    val selection: Selector = "::selection"
    selection should be === Pseudo("::selection")
  }
}