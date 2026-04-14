package org.nspl
import java.awt.{Font => JFont}

private[nspl] object JavaFontConversion {

  /** Logical font names that Java maps to composite fonts.
    * VectorGraphics2D hangs on macOS when trying to resolve these
    * to physical fonts during PDF/SVG/EPS generation.
    */
  private val logicalFontNames = Set(
    "sansserif", "serif", "monospaced", "dialog", "dialoginput"
  )

  /** Cache of logical font name -> resolved physical font family name */
  private lazy val physicalFontMap: Map[String, String] = {
    val ge = java.awt.GraphicsEnvironment.getLocalGraphicsEnvironment()
    val available = ge.getAvailableFontFamilyNames()
    logicalFontNames.flatMap { logical =>
      val jf = new JFont(logical, JFont.PLAIN, 12)
      val family = jf.getFamily()
      if (!logicalFontNames.contains(family.toLowerCase)) {
        Some(logical -> family)
      } else {
        val fallback = logical match {
          case "sansserif" | "dialog" | "dialoginput" =>
            available.find(f => f == "Helvetica" || f == "Arial" || f == "Liberation Sans")
          case "serif" =>
            available.find(f => f == "Times" || f == "Times New Roman" || f == "Liberation Serif")
          case "monospaced" =>
            available.find(f => f == "Courier" || f == "Courier New" || f == "Liberation Mono")
          case _ => None
        }
        fallback.map(logical -> _)
      }
    }.toMap
  }

  private def resolvePhysicalName(name: String): String = {
    val lower = name.toLowerCase
    if (logicalFontNames.contains(lower))
      physicalFontMap.getOrElse(lower, name)
    else name
  }

  def font2font(myFont: Font): JFont =
    new JFont(resolvePhysicalName(myFont.name), JFont.PLAIN, myFont.size)

}

private[nspl] object AwtGlyphMeasurer extends Font.GlyphMeasurer {
  import JavaFontConversion._
  import java.awt.image.BufferedImage
  val bimage = new BufferedImage(50, 50, BufferedImage.TYPE_BYTE_BINARY)
  val g2d = bimage.createGraphics();
  val frc = g2d.getFontRenderContext
  val abc =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPRQRSTUVWXYZ0123456789%,./][()]"
  def advance(s: Char, f: Font): Double =
    font2font(f).getStringBounds(s.toString, frc).getWidth
  def lineMetrics(f: Font): Font.LineMetrics = {
    val lm = font2font(f).getLineMetrics(abc, frc)
    Font.LineMetrics(lm.getAscent, lm.getDescent, lm.getLeading)
  }
}
