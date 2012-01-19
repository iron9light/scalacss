package iron9light.tools.scalacss

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import CssDSL._

/**
 * @author il
 * @version 11/16/11 9:19 PM
 */

@RunWith(classOf[JUnitRunner])
class CssDSLSuite extends FunSuite {
  test("smoke") {
    val c = ("div" :: ">" :: "a" :: Nil) % {
      ("color" := "#FFFFFF") ~
        ("aa" := "bb") ~
        ("::selection" % {
          "cc" := "dd"
        })
    }

    println(Printer.pretty(CssAST.render(c)))

    println()

    println(Printer.pretty(CssAST.render(CssAST.optimize(c))))
  }
}