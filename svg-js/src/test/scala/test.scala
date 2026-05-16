import org.nspl._
import svgrenderer._
import org.scalajs.dom.document

import scala.scalajs.js.annotation._

/** Interactive SVG demo. Mirrors `canvas/src/test/scala/test.scala` so the
  * two backends can be exercised side-by-side. The DOM scaffolding lives in
  * `svg-js/index.html`; this object only attaches the rendered SVG nodes
  * to the placeholder elements and routes interaction callbacks to the
  * `#event-log` `<pre>`. Build with `sbt svgJs/Test/fastLinkJS` and open
  * `svg-js/index.html`.
  */
@JSExportTopLevel("nsplsvgtest")
object nsplsvgtest {

  private def buildScatter() = {
    val rng = new scala.util.Random(42)
    val data = (1 to 200).map { i =>
      val cluster = i / 50
      (
        rng.nextGaussian() + cluster * 3d,
        rng.nextGaussian() + cluster * 2d,
        cluster.toDouble
      )
    }
    xyplot(
      data -> point(
        color = DiscreteColors(8),
        size = 5d,
        noIdentifier = false
      )
    )(par.xlab("x").ylab("y"))
  }

  private def buildLine() = xyplot(
    (1 to 50).map(i => (i.toDouble, math.sin(i * 0.2))) -> line()
  )(par.xlab("i").ylab("sin"))

  private def fmtId(id: Identifier): String = id match {
    case DataRowIdx(ext, ds, row) =>
      s"DataRow(externalDS=$ext, ds=$ds, row=$row)"
    case TextBoxIdentifier(label, idx) =>
      s"TextBox(label=$label, index=$idx)"
    case _: PlotAreaIdentifier        => "PlotArea"
    case EmptyIdentifier              => "Empty"
    case other                        => other.toString
  }

  @JSExport
  def bind(): Unit = {
    val scatterSlot = document.getElementById("scatter-plot")
    val lineSlot = document.getElementById("line-plot")
    val log = document.getElementById("event-log")

    if (scatterSlot == null || lineSlot == null || log == null) {
      throw new RuntimeException(
        "nsplsvgtest: expected DOM elements #scatter-plot, #line-plot, #event-log"
      )
    }

    def emit(line: String): Unit = {
      val p = document.createElement("div")
      p.textContent = line
      log.insertBefore(p, log.firstChild)
      while (log.childNodes.length > 200) log.removeChild(log.lastChild)
    }

    val (scatterSvg, _) = render(
      buildScatter(),
      width = 600,
      height = 400,
      onShapeClick = Some { (id, _, _) => emit(s"click  ${fmtId(id)}") },
      onHover = Some { (id, _, _) => emit(s"hover  ${fmtId(id)}") },
      onUnhover = Some { (id, _, _) => emit(s"unhover ${fmtId(id)}") },
      onSelection = Some { ids =>
        val sample = ids.take(8).map(fmtId).mkString(", ")
        val tail = if (ids.size > 8) " …" else ""
        emit(s"select ${ids.size} shapes  → $sample$tail")
      },
      enableCrosshair = true
    )
    val (lineSvg, _) = render(
      buildLine(),
      width = 600,
      height = 200,
      click = id => emit(s"plotMouseDown ${fmtId(id)}"),
      enableCrosshair = true
    )

    scatterSlot.appendChild(scatterSvg)
    lineSlot.appendChild(lineSvg)
    emit("ready")
  }
}
