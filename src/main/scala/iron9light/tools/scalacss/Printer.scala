package iron9light.tools.scalacss

object Printer extends Printer

trait Printer {
  import java.io._
  import scala.text._

  /**Compact printing (no whitespace etc.)
   */
  def compact(doc: Document): String = compact(doc, new StringWriter).toString

  /**Compact printing (no whitespace etc.)
   */
  def compact[A <: Writer](doc: Document, out: A): A = {
    def layout(docs: List[Document]) {
      docs match {
        case Nil =>
        case DocText(" ") :: rs => layout(rs)
        case DocText(s) :: rs => out.write(s); layout(rs)
        case DocCons(d1, d2) :: rs => layout(d1 :: d2 :: rs)
        case DocBreak :: rs => layout(rs)
        case DocNest(_, d) :: rs => layout(d :: rs)
        case DocGroup(d) :: rs => layout(d :: rs)
        case DocNil :: rs => layout(rs)
      }
    }

    layout(List(doc))
    out.flush()
    out
  }

  /**Pretty printing.
   */
  def pretty(doc: Document): String = pretty(doc, new StringWriter).toString

  /**Pretty printing.
   */
  def pretty[A <: Writer](doc: Document, out: A): A = {
    doc.format(0, out)
    out
  }
}



