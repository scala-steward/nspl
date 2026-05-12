# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

nspl is a 2D scientific plotting library for Scala and Scala.js. It supports scatter, line, contour, raster, bar, box, histogram, and density plots with rendering backends for JVM (AWT/PDF/SVG/EPS) and browser (HTML5 Canvas/SVG).

## Build Commands

```bash
sbt -J-Xmx3000m +compile              # Compile all modules for both Scala versions
sbt saddle/test                        # Run Saddle integration tests (the main test suite)
sbt awt/test                           # Run AWT rendering tests
sbt versionPolicyCheck                 # Enforce binary compatibility (run by CI)
sbt scalafmtAll                        # Format all sources with Scalafmt
sbt docs/mdoc docs/unidoc             # Generate documentation
```

CI runs: `sbt -J-Xmx3000m +compile saddle/test versionPolicyCheck`

## Module Structure

- **core** / **coreJS** — Platform-agnostic plotting logic (scene graph, axes, data rendering, layouts). No external dependencies.
- **sharedJvm** / **sharedJs** — Shared utilities bridging core to platform-specific code.
- **awt** — JVM rendering via Java AWT Graphics2D. Exports PNG, JPG, PDF, SVG, EPS. Uses VectorGraphics2D.
- **canvas** — Scala.js rendering to HTML5 Canvas with interactive pan/zoom.
- **scalatagsJs** — Scala.js SVG rendering via ScalaTags.
- **saddle** / **saddleJS** — Integration with the Saddle dataframe library.
- **docs** / **jsdocs** — Documentation (mdoc + Hugo website).

## Architecture

**Scene graph pattern**: All visual elements implement `Renderable[K]` (transform, bounds, translate, scale, rotate). A `RenderingContext[A]` manages transform state, and `Renderer[E, R]` type-safely connects element types to rendering contexts. Platform backends (AWT, Canvas, SVG) provide implicit `Renderer` instances.

**Data pipeline**: `DataSource` provides lazy, iterator-based data access. `DataRenderer` implementations (`point()`, `line()`, `bar()`, `boxwhisker()`, `area()`) convert rows into scene graph elements. High-level entry points are in `simpleplots.scala`: `xyplot()`, `boxplot()`, `rasterplot()`, `xyzplot()`.

**Heterogeneous composition**: `ElemList`, `ElemList2`, `Elems2`..`Elems5` (generated via sbt-boilerplate) compose mixed renderable types using `Either`.

**Configuration**: Immutable `Parameters` case class with builder-style copy methods (e.g., `par.xlog(true).main("title")`).

**Interaction model**: User input flows through `Event` types in `events.scala` (`Scroll`, `Drag`, `Selection`, `MouseHover`, `MouseLeave`). The canvas backend hit-tests, builds the event, and feeds it into the plot's `Build` partial function (`xyplotareaBuild`), which returns a fresh `XYPlotArea` for repainting. Live hover decorations (e.g. the crosshair) live *inside* the plot's scene graph — they're parameters on `xyplotarea` toggled by `MouseHover`/`MouseLeave` rebuilds, not overlays drawn by the backend. `PlotAreaIdentifier` carries the axes and view-space frame `Bounds` so `mouseToWorld(canvasPoint)` can invert canvas pixels back to data coordinates. `EventFusionHelper` collapses high-frequency events (consecutive Drags, Hovers, growing Selections) in the replay log.

## Key Source Locations

- Core abstractions: `core/src/main/scala/org/nspl/core.scala`
- High-level plot API: `core/src/main/scala/org/nspl/simpleplots.scala`
- Data renderers: `core/src/main/scala/org/nspl/datarenderers.scala`
- AWT backend: `awt/src/main/scala/org/nspl/awt.scala`
- Events and replay-log fusion: `core/src/main/scala/org/nspl/events.scala`
- Interactivity tests (pure-JVM, exercise the event/Build math the canvas drives): `awt/src/test/scala/interaction.test.scala`

## Build Details

- Scala 2.13.16 and Scala 3.3.5 LTS cross-compilation
- `-Xfatal-warnings` is enabled — all warnings are errors
- Test framework: MUnit
- Versioning: early-semver with sbt-version-policy enforcement
