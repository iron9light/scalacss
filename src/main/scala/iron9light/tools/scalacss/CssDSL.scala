package iron9light.tools.scalacss

import CssAST._
import collection.generic.CanBuildFrom
import collection.TraversableLike
import CssDSL.SeqAssoc


/**
 * @author il
 * @version 11/16/11 4:36 PM
 */

trait Implicits {
  implicit def covertToSeq[To, From <% To](o: From): List[To] = o :: Nil

  implicit def covertSeq[TFrom, TTo, Repr, That](seq: TraversableLike[TFrom, Repr])(implicit converter: TFrom => TTo, bf: CanBuildFrom[Repr, TTo, That]) = seq.map(converter)

  implicit def converS[To, From <% To](seq: Seq[From]) = seq.map{
    x => {
      val y: To = x
      y
    }
  }

  implicit def toSelector(str: String) = {
    val P = """^[\+~>]$""".r
    str.trim match {
      case s@P() => Combinator(s)
      case s if s.startsWith(":") => Pseudo(s)
      case s => SelectorElement(s)
    }
  }

  implicit def toProperty(s: String) = CssProperty(s)

  implicit def toValue(s: String) = CssValue(s)

  implicit def toValueSeq[T <% CssValue](value: T): Seq[CssValue] = Seq(value)

  implicit def toSelectorSeq[T <% Selector](selector: T): Seq[Selector] = Seq(selector)

  implicit def toCssElementSeq[T <% CssElement](element: T): Seq[CssElement] = Seq(element)

  implicit def convertValueSeq[T <% CssValue](values: Seq[T]): Seq[CssValue] = converS(values)

  implicit def convertSelectorSeq[T <% Selector](selectors: Seq[T]): Seq[Selector] = converS(selectors)

  implicit def convertCssElementSeq[T <% CssElement](elements: Seq[T]): Seq[CssElement] = converS(elements)
}

object CssDSL extends Implicits {
  val CSel = new CSelAssoc

  val CVal = new CValAssoc

  val CElem = new CElemAssoc

  implicit def toRich[T](x: T) = new Rich(x)

  class Rich[T](val x: T) {
    // def s(implicit convert: T => Seq[Selector]) = new SelectorAssoc(x)

    def :=[V](values: V)
             (implicit convert: T => CssProperty, convert2: V => Seq[CssValue]) =
      Declaration(x, values)

    def ~[E](elements: E)
            (implicit convert: T => Seq[CssElement], convert2: E => Seq[CssElement]): Seq[CssElement] =
      x ++ elements

    def --[D](declarations: D)
            (implicit convert: T => CssProperty, convert2: D => Seq[CssElement]): Seq[Declaration] =
      for (Declaration(property, values) <- declarations) yield Declaration(x - property, values)

    def -[P](property: P)
            (implicit convert: T => CssProperty, convert2: P => CssProperty) =
      CssProperty(x.s + "-" + property.s)

    def %[B](declarationBlock: B)
             (implicit convert: T => Seq[Selector], convert2: B => Seq[CssElement]) =
      CssRule(x, declarationBlock)
  }

  implicit def toDeclaration[K <% CssProperty, V <% Seq[CssValue]](pair: (K, V)) =
    pair._1 := pair._2

  abstract class SeqAssoc[T, X <: SeqAssoc[T, X]](val seq: Seq[T]) {self: X =>
    protected def newInstance(s: Seq[T]): X

//    def %[B <% Seq[CssElement]](declarationBlock: B) =
//      CssRule(selector, declarationBlock)

    def :::[S <% Seq[T]](s: S) = {
      newInstance(s ++ seq)
    }

    def ::[S <% T](s: S) = newInstance(s :: seq.toList)
  }

  object CSelAssoc {
    implicit def toSeq(s: CSelAssoc) = s.seq
  }

  class CSelAssoc(seq: Seq[Selector] = Nil) extends SeqAssoc[Selector, CSelAssoc](seq) {
    def newInstance(s: Seq[Selector]) = new CSelAssoc(s)
  }

  object CValAssoc {
    implicit def toSeq(s: CValAssoc) = s.seq
  }

  class CValAssoc(seq: Seq[CssValue] = Nil) extends SeqAssoc[CssValue, CValAssoc](seq) {
    def newInstance(s: Seq[CssValue]) = new CValAssoc(s)
  }

  object CElemAssoc {
    implicit def toSeq(s: CElemAssoc) = s.seq
  }

  class CElemAssoc(seq: Seq[CssElement] = Nil) extends SeqAssoc[CssElement, CElemAssoc](seq) {
    def newInstance(s: Seq[CssElement]) = new CElemAssoc(s)
  }
}

//object CssDSL extends Implicits {
//  implicit def toStringAssoc(s: String) = new StringAssoc(s)
//
//  //  object StringAssoc {
//  //    implicit def apply(s: String) = new StringAssoc(s)
//  //  }
//
//  implicit def stringToSelectorAssoc(s: String) = {
//    new SelectorAssoc(s :: Nil)
//  }
//  implicit def stringSeqToSelectorAssoc(s: Seq[String]) = {
//    new SelectorAssoc(s)
//  }
//  implicit def toSelectorAssoc0(selector: Selector) = new SelectorAssoc(selector)
//  implicit def toSelectorAssoc(selector: Seq[Selector]) = new SelectorAssoc(selector)
//  class SelectorAssoc[T <% Selector](selector: Seq[T]) {
//    implicit def toSelector: Seq[Selector] = selector
//
//    def apply[B <% Seq[CssElement]](declarationBlock: => B) = CssRule(selector, declarationBlock)
//
//    //    def :::[S <% Seq[Selector]](otherSelector: S): Seq[Selector] = otherSelector ++ selector
//    //
//    //    def :::[S <% Seq[Selector]](otherSelector: S): Seq[Selector] = otherSelector ++ selector
//  }
//
//  implicit def toPropertyAssoc(property: CssProperty) = new PropertyAssoc(property)
//  implicit def stringToPropertyAssoc(s: String) = new PropertyAssoc(s)
//  class PropertyAssoc[T <% CssProperty](property: T) {
//    implicit def toProperty: CssProperty = property
//
//    def -[P <% CssProperty](subProperty: P) = CssProperty(property.s + "-" + subProperty.s)
//
//    def ->[V <% Seq[CssValue]](values: V) = Declaration(property, values)
//  }
//
////  implicit def toCssElementsAssoc0[T <% CssElement](element: T) = new CssElementsAssoc(element :: List[CssElement]())
////  implicit def toCssElementsAssoc[T <% Seq[CssElement]](elements: T) = new CssElementsAssoc(elements)
//
//  implicit def elementToCssElementsAssoc(s: CssElement) = new CssElementsAssoc(s)
//  implicit def elementsToCssElementsAssoc(s: Seq[CssElement]) = new CssElementsAssoc(s)
//  class CssElementsAssoc[T <% CssElement](elements: Seq[T]) {
//    implicit def toElements: Seq[CssElement] = elements
//
//    def ~[E <% Seq[CssElement]](element: E): Seq[CssElement] = elements ++ element
//  }
//
//  class StringAssoc(str: String) {
//    def s: Selector = str
//
//    def v: CssValue = str
//
//    def p: CssProperty = str
//  }
//}