package org.nspl

// Exclude `Event` and `Selection` from the wildcard import — they would
// shadow our `org.nspl.Event` and `org.nspl.Selection` under Scala 3.
import org.scalajs.dom.{Event => _, Selection => _, _}
import org.scalajs.dom
import scala.collection.mutable.ArrayBuffer
import java.util.ConcurrentModificationException

/** Rendering context for the SVG (Scala.js) backend. Mirrors the canvas
  * backend in interaction semantics: the same callbacks, the same event
  * fusion replay log, the same plot-area / hover / click / selection
  * registration model. The difference is the output medium — SVG elements
  * appended to a root `<svg>` node — and the hit-testing strategy, which
  * is pure 2D math (no `isPointInPath`).
  */
class SvgRC private[nspl] (
    private[nspl] val root: SVGSVGElement,
    private[nspl] val cick: Identifier => Unit,
    private[nspl] val onHover: Option[(Identifier, Point, MouseEvent) => Unit],
    private[nspl] val onUnhover: Option[(Identifier, Point, MouseEvent) => Unit],
    private[nspl] val onShapeClick: Option[
      (Identifier, Point, MouseEvent) => Unit
    ],
    private[nspl] val onSelection: Option[collection.Seq[Identifier] => Unit]
) extends RenderingContext[SvgRC] {

  private[nspl] var transform: AffineTransform = AffineTransform.identity

  type LocalTx = AffineTransform

  def localToScala(tx: AffineTransform): AffineTransform = tx

  def concatTransform(tx: AffineTransform): Unit = {
    transform = transform.applyBefore(tx)
  }

  def setTransform(tx: LocalTx): Unit = {
    transform = tx
  }

  def getTransform: LocalTx = transform

  private[nspl] val SvgNs = "http://www.w3.org/2000/svg"

  private[nspl] def makeElement(tag: String): Element =
    dom.document.createElementNS(SvgNs, tag)

  private[nspl] def appendChild(el: Element): Unit = {
    root.appendChild(el)
    ()
  }

  private[nspl] var mousedown = false
  private[nspl] var dragShift = false
  private[nspl] val plotAreaShapes = ArrayBuffer[(Shape, PlotAreaIdentifier)]()
  private[nspl] val clickShapes = ArrayBuffer[(Shape, Identifier)]()
  private[nspl] val hoverShapes = ArrayBuffer[(Shape, Identifier)]()
  private[nspl] val selectionShapes = ArrayBuffer[(Shape, Identifier)]()
  private[nspl] var lastHovered: collection.Seq[Int] = Vector.empty
  private[nspl] var lastHoveredPlotAreas: collection.Seq[Int] = Vector.empty

  private[nspl] def registerPlotArea(shape: Shape, id: PlotAreaIdentifier) =
    plotAreaShapes.append((shape, id))

  private[nspl] def registerOnClick(shape: Shape, id: Identifier) =
    clickShapes.append((shape, id))
  private[nspl] def registerOnHover(shape: Shape, id: Identifier) =
    hoverShapes.append((shape, id))
  private[nspl] def registerOnSelection(shape: Shape, id: Identifier) =
    selectionShapes.append((shape, id))

  private[nspl] def processPlotArea(p: Point)(
      cb: PlotAreaIdentifier => Unit
  ) = {
    hitTest[PlotAreaIdentifier](
      p,
      true,
      plotAreaShapes,
      (id, bounds) => cb(id.withBounds(bounds))
    )
  }

  /** Hit-test the registered hover shapes (and plot areas) at point `p`,
    * invoke `onHover` for each hit, and invoke `onUnhover` for shapes that
    * were hit on the previous call but are not hit this call.
    */
  private[nspl] def callHoverCallbackOnHit(e: MouseEvent, p: Point): Unit =
    onHover.foreach { hover =>
      val previousShapes = lastHovered
      val previousPlots = lastHoveredPlotAreas
      val currentShapes = hitTest[Identifier](
        p,
        false,
        hoverShapes,
        (id, _) => hover(id, p, e)
      )
      val currentPlots = hitTest[PlotAreaIdentifier](
        p,
        true,
        plotAreaShapes,
        (id, bounds) => hover(id.withBounds(bounds), p, e)
      )
      lastHovered = currentShapes
      lastHoveredPlotAreas = currentPlots
      onUnhover.foreach { unhover =>
        previousShapes.filterNot(currentShapes.contains).foreach { idx =>
          if (idx >= 0 && idx < hoverShapes.size)
            unhover(hoverShapes(idx)._2, p, e)
        }
        previousPlots.filterNot(currentPlots.contains).foreach { idx =>
          if (idx >= 0 && idx < plotAreaShapes.size)
            unhover(plotAreaShapes(idx)._2, p, e)
        }
      }
    }

  private[nspl] def callClickCallbackOnHit(e: MouseEvent, p: Point): Unit =
    onShapeClick.foreach { click =>
      hitTest[Identifier](
        p,
        false,
        clickShapes,
        (id, _) => click(id, p, e)
      )
    }

  private[nspl] def callSelectionCallback(p1: Point, p2: Point): Unit =
    onSelection.foreach { sel =>
      val rect = Bounds.fromPoints(p1, p2)
      val hits = selectionShapes
        .filter { case (sh, _) =>
          val b = sh.bounds
          rect.contains(Point(b.centerX, b.centerY))
        }
        .map(_._2)
      sel(hits)
    }

  /** Iterate shapes, hit-test each, fire callback on hits, return the
    * indices of shapes that were hit. Indices are useful so the caller
    * can compare against a previous call's hit set (for unhover).
    */
  private def hitTest[T](
      p: Point,
      needsTransformedBounds: Boolean,
      shapes: collection.Seq[(Shape, T)],
      callback: (T, Option[Bounds]) => Unit
  ): collection.Seq[Int] = {
    val out = ArrayBuffer.empty[Int]
    var idx = 0
    val n = shapes.size
    while (idx < n) {
      val (shape, id) = shapes(idx)
      val (hit, transformedBounds) = withTransform(shape.currentTransform) {
        val tb =
          if (needsTransformedBounds)
            id match {
              case pl: PlotAreaIdentifier =>
                pl.bounds.map(transform.transform)
              case _ => None
            }
          else None
        val isHit = svgrenderer.shapeContains(shape, p, transform)
        (isHit, tb)
      }
      if (hit) {
        callback(id, transformedBounds)
        out.append(idx)
      }
      idx += 1
    }
    out
  }

  def clear() = {
    while (root.firstChild != null) {
      root.removeChild(root.firstChild)
      ()
    }
    transform = AffineTransform.identity
    plotAreaShapes.clear()
    clickShapes.clear()
    hoverShapes.clear()
    selectionShapes.clear()
  }

}

object svgrenderer {

  implicit val defaultGlyphMeasurer: Font.GlyphMeasurer = CanvasGlyphMeasurer

  implicit val defaultFont: FontConfiguration = font("Arial")

  private[nspl] def cssColor(c: Color) =
    s"rgba(${c.r},${c.g},${c.b},${c.a})"

  /** SVG matrix() attribute from an nspl AffineTransform. SVG uses the
    * same column ordering as canvas: `matrix(a b c d e f)` where
    * `(a,b,c,d,e,f) = (m0,m3,m1,m4,m2,m5)` — i.e. the second column lists
    * the `b,d,f` row, which matches what canvas's `setTransform` expects.
    */
  private[nspl] def matrix(tx: AffineTransform): String =
    s"matrix(${tx.m0} ${tx.m3} ${tx.m1} ${tx.m4} ${tx.m2} ${tx.m5})"

  /** Convert a DOM `MouseEvent` to SVG-space coordinates by subtracting
    * the SVG element's bounding rect. Unlike the canvas backend, no
    * `devicePixelRatio` factor is needed — SVG is resolution-independent
    * and the `viewBox` matches the element's pixel size.
    */
  def getSvgCoordinate(svg: SVGSVGElement, e: MouseEvent): Point = {
    val rect = svg.getBoundingClientRect()
    val x = e.clientX - rect.left
    val y = e.clientY - rect.top
    Point(x, y)
  }

  /** Hit-test using pure 2D math (no canvas dependency). Rectangle,
    * Ellipse, Line, and SimplePath are exact; Path with curves falls back
    * to its axis-aligned bounding box, which is adequate for typical
    * hover targets and avoids dragging in a canvas just for hit testing.
    *
    * The accumulated `tx` is the transform that was in effect when the
    * shape was originally drawn; we invert it once to bring the screen
    * point back into the shape's local coordinate system.
    */
  private[nspl] def shapeContains(
      shape: Shape,
      p: Point,
      tx: AffineTransform
  ): Boolean = {
    val inv = tx.inverse
    val local = inv.transform(p)
    shape match {
      case Rectangle(x, y, w, h, _, _) =>
        local.x >= x && local.x <= x + w && local.y >= y && local.y <= y + h
      case Ellipse(x, y, w, h, _) =>
        val cx = x + 0.5 * w
        val cy = y + 0.5 * h
        val rx = 0.5 * w
        val ry = 0.5 * h
        if (rx == 0d || ry == 0d) false
        else {
          val dx = (local.x - cx) / rx
          val dy = (local.y - cy) / ry
          dx * dx + dy * dy <= 1d
        }
      case ln: Line =>
        val dx = ln.x2 - ln.x1
        val dy = ln.y2 - ln.y1
        val len2 = dx * dx + dy * dy
        if (len2 == 0d) {
          val ex = local.x - ln.x1
          val ey = local.y - ln.y1
          ex * ex + ey * ey <= 4d
        } else {
          val t =
            ((local.x - ln.x1) * dx + (local.y - ln.y1) * dy) / len2
          val tc = if (t < 0d) 0d else if (t > 1d) 1d else t
          val px = ln.x1 + tc * dx
          val py = ln.y1 + tc * dy
          val ex = local.x - px
          val ey = local.y - py
          ex * ex + ey * ey <= 4d
        }
      case sp: SimplePath => pointInPolygon(sp.ps, local)
      case path: Path =>
        val b = path.bounds
        local.x >= b.x && local.x <= b.x + b.w &&
        local.y >= b.y && local.y <= b.y + b.h
    }
  }

  private def pointInPolygon(ps: Seq[Point], p: Point): Boolean = {
    val arr = ps.toArray
    if (arr.length < 3) false
    else {
      var inside = false
      var j = arr.length - 1
      var i = 0
      while (i < arr.length) {
        val xi = arr(i).x
        val yi = arr(i).y
        val xj = arr(j).x
        val yj = arr(j).y
        val denom = yj - yi
        if (denom != 0d) {
          val intersects = ((yi > p.y) != (yj > p.y)) &&
            (p.x < (xj - xi) * (p.y - yi) / denom + xi)
          if (intersects) inside = !inside
        }
        j = i
        i += 1
      }
      inside
    }
  }

  /** Render a `Build[K]` into a fresh `<svg>` element and return the
    * element plus an updater function. The callbacks mirror the canvas
    * backend; see [[canvasrenderer.render]] for details on each one.
    */
  def render[K <: Renderable[K]](
      build0: Build[K],
      width: Int,
      height: Int,
      click: Identifier => Unit = (_ => ()),
      onHover: Option[(Identifier, Point, MouseEvent) => Unit] = None,
      onUnhover: Option[(Identifier, Point, MouseEvent) => Unit] = None,
      onShapeClick: Option[(Identifier, Point, MouseEvent) => Unit] = None,
      onSelection: Option[collection.Seq[Identifier] => Unit] = None,
      enableScroll: Boolean = true,
      enableDrag: Boolean = true,
      enableCrosshair: Boolean = false
  )(implicit
      er: Renderer[K, SvgRC]
  ): (SVGSVGElement, Build[K] => Unit) = {

    val svg = dom.document
      .createElementNS("http://www.w3.org/2000/svg", "svg")
      .asInstanceOf[SVGSVGElement]
    svg.setAttribute("width", s"${width}px")
    svg.setAttribute("height", s"${height}px")
    svg.setAttribute("viewBox", s"0 0 $width $height")
    // Without this, the default `visiblePainted` would only fire mouse
    // events over painted children — pan / zoom / hover in the empty
    // interior of a plot area would silently drop. Canvas has no
    // equivalent issue because the whole canvas is one element.
    svg.setAttribute("pointer-events", "all")

    val ctx =
      new SvgRC(
        svg,
        click,
        onHover,
        onUnhover,
        onShapeClick,
        onSelection
      )

    var build = build0
    var paintableElem = build.build
    var dragStart = Point(0, 0)
    var queuedCallback: Double => Unit = null
    val eventStore = new EventFusionHelper
    var lastHoveredPlotArea: Option[PlotAreaIdentifier] = None

    def paintBounds = {
      // Fit-inside: scale the plot so it fits entirely within the
      // viewBox without distorting its aspect.
      val plotAspect = paintableElem.bounds.h / paintableElem.bounds.w
      val canvasAspect = height.toDouble / width.toDouble
      if (plotAspect > canvasAspect) {
        val w = (height / plotAspect).toInt
        Bounds(0, 0, w, height)
      } else {
        val h = (width * plotAspect).toInt
        Bounds(0, 0, width, h)
      }
    }

    def queueAnimationFrame(body: Double => Unit) = {
      if (queuedCallback == null) {
        dom.window.requestAnimationFrame { d =>
          val cb = queuedCallback
          queuedCallback = null
          try {
            cb(d)
          } catch {
            case _: ConcurrentModificationException => ()
          }
        }
      }
      queuedCallback = body
    }

    def paint() = {
      ctx.clear()
      ctx.render(
        fitToBounds(paintableElem, paintBounds)
      )
    }

    def onmousedown(e: MouseEvent) = {
      if (e.button == 0) {
        e.preventDefault()
        val p = getSvgCoordinate(svg, e)
        ctx.processPlotArea(p) { identifier =>
          ctx.mousedown = true
          ctx.dragShift = e.shiftKey
          dragStart = p
          click(identifier)
        }
      }
    }

    def onmouseup(e: MouseEvent) = {
      if (e.button == 0 && ctx.mousedown) {
        e.preventDefault()
        val p = getSvgCoordinate(svg, e)
        if (ctx.dragShift && onSelection.isDefined) {
          ctx.callSelectionCallback(dragStart, p)
        }
      }
      ctx.mousedown = false
      ctx.dragShift = false
    }

    def onclickHandler(e: MouseEvent) = {
      if (e.button == 0 && onShapeClick.isDefined) {
        val p = getSvgCoordinate(svg, e)
        dom.window.requestAnimationFrame { _ =>
          ctx.callClickCallbackOnHit(e, p)
        }
      }
    }

    def onmove(e: MouseEvent) = {
      val p = getSvgCoordinate(svg, e)
      if (e.buttons == 0 && (onHover.isDefined || enableCrosshair)) {
        queueAnimationFrame { _ =>
          if (onHover.isDefined) {
            ctx.callHoverCallbackOnHit(e, p)
          }
          if (enableCrosshair) {
            var hit = false
            ctx.processPlotArea(p) { id =>
              hit = true
              val event = MouseHover(p, id)
              paintableElem = build(Some(paintableElem) -> event)
              paint()
              eventStore.add(event, id.canFuseEvents)
              lastHoveredPlotArea = Some(id)
            }
            if (!hit) {
              lastHoveredPlotArea.foreach { id =>
                val event = MouseLeave(id)
                paintableElem = build(Some(paintableElem) -> event)
                paint()
                eventStore.add(event, id.canFuseEvents)
              }
              lastHoveredPlotArea = None
            }
          }
        }
      }
      if (e.button == 0 && ctx.mousedown) {
        e.preventDefault()
        queueAnimationFrame { _ =>
          val v = Point(dragStart.x - p.x, dragStart.y - p.y)
          val l = math.sqrt(v.x * v.x + v.y * v.y)
          if (l > 0) {
            ctx.processPlotArea(p) { id =>
              val ev =
                if (ctx.dragShift && onSelection.isDefined)
                  Some(Selection(dragStart, p, id))
                else if (enableDrag)
                  Some(Drag(dragStart, p, id))
                else None
              ev match {
                case Some(event) =>
                  paintableElem = build(Some(paintableElem) -> event)
                  if (!ctx.dragShift) dragStart = p
                  paint()
                  eventStore.add(event, id.canFuseEvents)
                case None => ()
              }
            }
          }
        }
      }
    }

    def onwheel(e: MouseEvent) = {
      if (enableScroll) {
        e.preventDefault()
        queueAnimationFrame { _ =>
          val p = getSvgCoordinate(svg, e)
          ctx.processPlotArea(p) { id =>
            val event = Scroll(
              e.asInstanceOf[scala.scalajs.js.Dynamic]
                .deltaY
                .asInstanceOf[Double],
              p,
              id
            )
            paintableElem = build(Some(paintableElem) -> event)
            paint()
            eventStore.add(event, id.canFuseEvents)
          }
        }
      }
    }

    val update: Build[K] => Unit = (k: Build[K]) =>
      queueAnimationFrame { _ =>
        build = k
        paintableElem = k.build
        eventStore.get.foreach { (event: org.nspl.Event) =>
          paintableElem = build(Some(paintableElem) -> event)
        }
        paint()
      }

    svg.onmousedown = onmousedown _
    svg.onmouseup = onmouseup _
    svg.onmousemove = onmove _
    if (onShapeClick.isDefined) {
      svg.onclick = onclickHandler _
    }
    svg.addEventListener("wheel", onwheel _)
    if (enableCrosshair) {
      svg.addEventListener(
        "mouseleave",
        (_: MouseEvent) =>
          lastHoveredPlotArea.foreach { id =>
            val event = MouseLeave(id)
            paintableElem = build(Some(paintableElem) -> event)
            paint()
            eventStore.add(event, id.canFuseEvents)
            lastHoveredPlotArea = None
          }
      )
    }

    queueAnimationFrame { _ =>
      paint()
    }

    (svg, update)

  }

  /** Build an SVG `<path>` `d` attribute from a list of `PathOperation`s.
    */
  private[nspl] def pathD(ops: Seq[PathOperation]): String = {
    val sb = new StringBuilder
    ops.foreach {
      case op: PathOperation.MoveTo =>
        sb.append("M ").append(op.p.x).append(' ').append(op.p.y).append(' ')
      case op: PathOperation.LineTo =>
        sb.append("L ").append(op.p.x).append(' ').append(op.p.y).append(' ')
      case op: PathOperation.QuadTo =>
        sb.append("Q ")
          .append(op.p1.x).append(' ').append(op.p1.y).append(' ')
          .append(op.p2.x).append(' ').append(op.p2.y).append(' ')
      case op: PathOperation.CubicTo =>
        sb.append("C ")
          .append(op.p1.x).append(' ').append(op.p1.y).append(' ')
          .append(op.p2.x).append(' ').append(op.p2.y).append(' ')
          .append(op.p3.x).append(' ').append(op.p3.y).append(' ')
    }
    sb.toString
  }

  /** Allocate an SVG element matching a shape's geometry. Fill/stroke
    * attributes are applied by the caller.
    */
  private[nspl] def makeShapeNode(ctx: SvgRC, sh: Shape): Element = sh match {
    case r: Rectangle =>
      val n = ctx.makeElement("rect")
      n.setAttribute("x", r.x.toString)
      n.setAttribute("y", r.y.toString)
      n.setAttribute("width", r.w.toString)
      n.setAttribute("height", r.h.toString)
      n
    case e: Ellipse =>
      val n = ctx.makeElement("ellipse")
      n.setAttribute("cx", (e.x + 0.5 * e.w).toString)
      n.setAttribute("cy", (e.y + 0.5 * e.h).toString)
      n.setAttribute("rx", (0.5 * e.w).toString)
      n.setAttribute("ry", (0.5 * e.h).toString)
      n
    case l: Line =>
      val n = ctx.makeElement("line")
      n.setAttribute("x1", l.x1.toString)
      n.setAttribute("y1", l.y1.toString)
      n.setAttribute("x2", l.x2.toString)
      n.setAttribute("y2", l.y2.toString)
      n
    case sp: SimplePath =>
      val n = ctx.makeElement("polyline")
      val pts = sp.ps.iterator.map(p => s"${p.x},${p.y}").mkString(" ")
      n.setAttribute("points", pts)
      n
    case p: Path =>
      val n = ctx.makeElement("path")
      n.setAttribute("d", pathD(p.path))
      n
  }

  implicit val shapeRenderer: Renderer[ShapeElem, SvgRC] =
    new Renderer[ShapeElem, SvgRC] {

      private def emit(ctx: SvgRC, elem: ShapeElem): Unit = {
        if (
          elem.fill.a > 0 || (elem.stroke.isDefined && elem.strokeColor.a > 0)
        ) {
          val node = makeShapeNode(ctx, elem.shape)
          node.setAttribute("transform", matrix(ctx.getAffineTransform))

          if (elem.fill.a > 0) {
            node.setAttribute("fill", cssColor(elem.fill))
          } else {
            node.setAttribute("fill", "none")
          }

          if (elem.stroke.isDefined && elem.strokeColor.a > 0) {
            val s = elem.stroke.get
            node.setAttribute("stroke", cssColor(elem.strokeColor))
            node.setAttribute("stroke-width", s.width.toString)
            if (s.dash.nonEmpty) {
              node.setAttribute(
                "stroke-dasharray",
                s.dash.mkString(" ")
              )
            }
            s.cap match {
              case Cap.Butt   => ()
              case Cap.Round  => node.setAttribute("stroke-linecap", "round")
              case Cap.Square => node.setAttribute("stroke-linecap", "square")
            }
          } else {
            node.setAttribute("stroke", "none")
          }

          ctx.appendChild(node)
        }
      }

      def render(ctx: SvgRC, elem: ShapeElem): Unit = {
        ctx.withTransform(elem.tx applyBefore elem.shape.currentTransform) {

          emit(ctx, elem)

          elem.identifier match {
            case EmptyIdentifier => ()
            case pa: PlotAreaIdentifier =>
              ctx.registerPlotArea(
                elem.shape.transform((_, old) =>
                  ctx.getAffineTransform.applyBefore(old)
                ),
                pa.withBounds(Some(elem.bounds))
              )
            case other =>
              val resolved = elem.shape.transform((_, old) =>
                ctx.getAffineTransform.applyBefore(old)
              )
              ctx.registerOnClick(resolved, other)
              ctx.registerOnHover(resolved, other)
              ctx.registerOnSelection(resolved, other)
          }
        }
      }
    }

  implicit val textRenderer: Renderer[TextBox, SvgRC] =
    new Renderer[TextBox, SvgRC] {

      def render(ctx: SvgRC, elem: TextBox): Unit = {
        if (!elem.layout.isEmpty) {
          ctx.withTransform(elem.tx) {

            // Sub/super script and underline are emulated the same way
            // as the canvas backend so glyph metrics line up: a smaller
            // font with a vertical offset, and an explicit underline rect.
            val sub = elem.subScript && !elem.superScript
            val sup = elem.superScript && !elem.subScript
            val scaledFont =
              if (sub || sup)
                new Font(
                  elem.font.name,
                  math.max(1, (elem.font.size * 0.7).toInt)
                )
              else elem.font

            val ascent =
              if (sub || sup) elem.font.size.toDouble * 0.7 else 0d
            val yShift =
              if (sup) -ascent * 0.4
              else if (sub) ascent * 0.4
              else 0d

            elem.layout.lines.foreach { case (line, lineTx) =>
              ctx.withTransform(lineTx) {
                val node = ctx.makeElement("text")
                node.setAttribute("transform", matrix(ctx.getAffineTransform))
                node.setAttribute("x", "0")
                node.setAttribute("y", yShift.toString)
                node.setAttribute("fill", cssColor(elem.color))
                node.setAttribute("font-family", scaledFont.name)
                node.setAttribute("font-size", scaledFont.size.toString)
                if (elem.bold) node.setAttribute("font-weight", "bold")
                if (elem.oblique) node.setAttribute("font-style", "italic")
                if (elem.underline)
                  node.setAttribute("text-decoration", "underline")
                node.textContent = line
                ctx.appendChild(node)
              }
            }

            elem.identifier match {
              case EmptyIdentifier => ()
              case pa: PlotAreaIdentifier =>
                ctx.registerPlotArea(
                  Shape.rectangle(
                    elem.bounds.x,
                    elem.bounds.y,
                    elem.bounds.w,
                    elem.bounds.h
                  ),
                  pa.withBounds(Some(elem.bounds))
                )
              case other =>
                val rect = Shape.rectangle(
                  elem.bounds.x,
                  elem.bounds.y,
                  elem.bounds.w,
                  elem.bounds.h
                )
                ctx.registerOnClick(rect, other)
                ctx.registerOnHover(rect, other)
                ctx.registerOnSelection(rect, other)
            }
          }
        }
      }
    }
}
