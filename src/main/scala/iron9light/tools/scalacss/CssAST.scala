package iron9light.tools.scalacss

import text.Document._
import scala.text._

object CssAST {
  final val blank = text(" ")
  final val semicolon = text(";")
  final val colon = text(":")
  final val comma = text(",")

  abstract class Selector {
    def s: String
  }

  case class SelectorElement(s: String) extends Selector

  case class Pseudo(s: String) extends Selector

  case class Combinator(s: String) extends Selector

  abstract class CssElement

  case class Declaration(property: CssProperty, values: Seq[CssValue]) extends CssElement

  case class CssProperty(s: String)

  case class CssValue(s: String)

  case class CssRule(selectors: Seq[Selector], elements: Seq[CssElement]) extends CssElement

  def render(elements: Seq[CssElement]): Document = {
    if (elements.isEmpty)
      empty
    else
      elements.map(renderElement).reduce(_ :/: _)
  }

  def render(element: CssElement): Document = renderElement(element)

  private def renderElement(element: CssElement): Document = element match {
    case declaration: Declaration => renderDeclaration(declaration)
    case cssRule: CssRule => renderCssRule(cssRule)
  }

  private def renderDeclaration(declaration: Declaration): Document = declaration match {
    case Declaration(property, values) => property.s :: colon :: blank :: renderValues(values)
  }

  private def renderValues(values: Seq[CssValue]): Document = {
    values.foldRight(semicolon) {
      case (value, `semicolon`) =>
        value.s :: semicolon
      case (value, doc) => value.s :: comma :: blank :: doc
    }
  }

  private def renderCssRule(cssRule: CssRule): Document = cssRule match {
    case CssRule(Nil, elements) =>
      render(elements)
    case CssRule(selectors, elements) =>
      val nested = break :: render(elements)
      renderSelector(selectors) :: blank :: "{" :: nest(2, nested) :: break :: text("}")
  }

  private def renderSelector(selectors: Seq[Selector]): Document = {
    //    val str = selectors.foldLeft("") {
    //      case ("", selector) => selector.s
    //      case (s, selectorElement: SelectorElement) => s + " " + selectorElement.s
    //      case (s, pseudoClass: Pseudo) => s + pseudoClass.s
    //    }
    //    text(str)

    selectors.foldRight[(Document, AnyRef)]((empty, Pseudo)) {
      case (Combinator(s), (DocNil, _)) =>
        (text(s), Combinator)
      case (SelectorElement(s), (DocNil, _)) =>
        (text(s), SelectorElement)
      case (Pseudo(s), (DocNil, _)) =>
        (text(s), Pseudo)
      case (Combinator(s), (doc, _)) =>
        (s :: blank :: doc, Combinator)
      case (SelectorElement(s), (doc, SelectorElement)) =>
        ((s + " ") :: doc, SelectorElement)
      case (SelectorElement(s), (doc, Combinator)) =>
        (s :: blank :: doc, SelectorElement)
      case (Pseudo(s), (doc, Combinator)) =>
        (s :: blank :: doc, Pseudo)
      case (SelectorElement(s), (doc, Pseudo)) =>
        (s :: doc, SelectorElement)
      case (Pseudo(s), (doc, Pseudo)) =>
        (s :: doc, Pseudo)
    }._1
  }

  def flatten(elements: Seq[CssElement]): Seq[CssElement] = {
    def shortenSelector(selectors: Seq[Selector]) = selectors.filterNot(_.s.trim.isEmpty)

    elements.flatMap {
      case declaration: Declaration => declaration :: Nil
      case (cssRule: CssRule) =>
        val selector = shortenSelector(cssRule.selectors)
        if (selector.isEmpty)
          flatten(cssRule.elements)
        else
          flatten(cssRule.elements).map {
            case declaration: Declaration => CssRule(selector, declaration :: Nil)
            case innerCssRule: CssRule =>
              CssRule(selector ++ innerCssRule.selectors, innerCssRule.elements)
          }
    }
  }

  def optimize(elements: Seq[CssElement]): Seq[CssElement] = {
    val (declarations, cssRules) = flatten(elements).partition(_.isInstanceOf[Declaration])
    //    val orderedDeclarations = (declarations.asInstanceOf[Seq[Declaration]].reverse.groupBy(_.property).map {
    //      case (property, ds) =>
    //        val values = ds.flatMap(_.values).distinct
    //        Declaration(property, values)
    //    }).toList.reverse
    val orderedDeclarations = declarations.asInstanceOf[Seq[Declaration]].toList.reverse.distinct.reverse

    val orderedRules = {
      val reversed = cssRules.asInstanceOf[Seq[CssRule]].reverse
      val group = reversed.groupBy(_.selectors)

      reversed.map {
        case CssRule(selector, _) =>
//          val elements = optimize(group(selector).reverse.flatMap(_.elements).reverse)
          val elements = optimize(group(selector).reverse.flatMap(_.elements))
          CssRule(selector, elements)
      }
    }.toList.distinct.reverse

    orderedDeclarations ::: orderedRules
  }
}