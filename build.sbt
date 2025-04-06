scalaVersion := "2.13.16"

ThisBuild / versionScheme := Some("early-semver")

ThisBuild / versionPolicyIntention := Compatibility.None
ThisBuild / versionPolicyIgnoredInternalDependencyVersions := Some(
  "^\\d+\\.\\d+\\.\\d+\\+\\d+".r
) // Support for versions generated by sbt-dynver

inThisBuild(
  List(
    organization := "io.github.pityka",
    homepage := Some(url("https://pityka.github.io/nspl/")),
    licenses := List(("MIT", url("https://opensource.org/licenses/MIT"))),
    developers := List(
      Developer(
        "pityka",
        "Istvan Bartha",
        "bartha.pityu@gmail.com",
        url("https://github.com/pityka/nspl")
      )
    )
  )
)

lazy val commonSettings = Seq(
  scalaVersion := "2.13.10",
  crossScalaVersions := Seq("2.13.10", "3.3.5"),
  javacOptions ++= Seq("-Xdoclint:none"),
  scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((3, _)) =>
      Seq(
        "-no-indent",
        "-old-syntax",
        "-deprecation", // Emit warning and location for usages of deprecated APIs.
        "-encoding",
        "utf-8", // Specify character encoding used by source files.
        "-feature", // Emit warning and location for usages of features that should be imported explicitly.
        "-language:postfixOps",
        "-language:existentials",
        "-language:implicitConversions",
        "-unchecked", // Enable additional warnings where generated code depends on assumptions.
        "-Xfatal-warnings" // Fail the compilation if there are any warnings.
      )
    case Some((2, _)) =>
      Seq(
        "-deprecation", // Emit warning and location for usages of deprecated APIs.
        "-encoding",
        "utf-8", // Specify character encoding used by source files.
        "-feature", // Emit warning and location for usages of features that should be imported explicitly.
        "-language:postfixOps",
        "-language:implicitConversions",
        "-unchecked", // Enable additional warnings where generated code depends on assumptions.
        "-Xfatal-warnings", // Fail the compilation if there are any warnings.
        "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
        "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
        "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
        "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
        "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
        "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
        "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
        "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
        "-Xlint:option-implicit", // Option.apply used implicit view.
        "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
        "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
        "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
        "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
        "-Ywarn-dead-code", // Warn when dead code is identified.
        // "-Ywarn-numeric-widen", // Warn when numerics are widened.
        "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
        "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
        "-Ywarn-unused:locals", // Warn if a local definition is unused.
        "-Ywarn-unused:params", // Warn if a value parameter is unused.
        "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
        "-Ywarn-unused:privates" // Warn if a private member is unused.
      )
    case _ => ???
  }),
  Compile / doc / scalacOptions ++= Seq(
    "-no-link-warnings" // Suppresses problems with Scaladoc
  ),
  Compile / doc / scalacOptions --= Seq(
    "-Xfatal-warnings"
  ),
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  fork := false
)

lazy val core = project
  .in(file("core"))
  .settings(commonSettings)
  .settings(
    name := "nspl-core"
  )
  .enablePlugins(spray.boilerplate.BoilerplatePlugin)

lazy val coreJS = project
  .in(file("core"))
  .settings(commonSettings)
  .settings(
    name := "nspl-core-js",
    target := file("core/targetJS"),
    Compile / sourceManaged := (Compile / sourceManaged).value.getAbsoluteFile
  )
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(spray.boilerplate.BoilerplatePlugin)

lazy val sharedJs = project
  .in(file("shared-js"))
  .settings(commonSettings)
  .settings(
    name := "nspl-shared-js",
    libraryDependencies += ("org.scala-js") %%% "scalajs-dom" % "2.8.0"
  )
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(coreJS)

lazy val canvas = project
  .in(file("canvas"))
  .settings(commonSettings)
  .settings(
    name := "nspl-canvas-js"
  )
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(coreJS, sharedJs)

lazy val scalatagsJs = project
  .in(file("scalatags-js"))
  .settings(commonSettings)
  .settings(
    name := "nspl-scalatags-js",
    libraryDependencies ++= Seq(
      ("com.lihaoyi") %%% "scalatags" % "0.11.0"
    )
  )
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(coreJS, sharedJs)

lazy val sharedJvm = project
  .in(file("shared-jvm"))
  .settings(commonSettings)
  .settings(
    name := "nspl-shared-jvm"
  )
  .dependsOn(core)

lazy val awt = project
  .in(file("awt"))
  .settings(commonSettings)
  .settings(
    name := "nspl-awt",
    libraryDependencies ++= Seq(
      "de.erichseifert.vectorgraphics2d" % "VectorGraphics2D" % "0.13",
      "org.scalameta" %% "munit" % "1.0.0-M1" % Test
    ),
    fork := true
  )
  .dependsOn(core, sharedJvm)

lazy val saddle = (project in file("saddle"))
  .settings(commonSettings)
  .settings(
    name := "nspl-saddle",
    libraryDependencies ++= Seq(
      "io.github.pityka" %% "saddle-core" % "4.0.0-M11",
      "org.scalameta" %% "munit" % "1.0.0" % Test
    )
  )
  .dependsOn(core, awt)

lazy val saddleJS = (project in file("saddle"))
  .settings(commonSettings)
  .settings(
    name := "nspl-saddle-js",
    target := file("saddle/targetJS"),
    libraryDependencies ++= Seq(
      "io.github.pityka" %%% "saddle-core" % "4.0.0-M11"
    ),
    Test / sources := Nil
  )
  .dependsOn(coreJS)
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(spray.boilerplate.BoilerplatePlugin)

publishArtifact := false

lazy val jsdocs = project
  .settings(
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.0.0"
  )
  .dependsOn(coreJS, canvas)
  .settings(
    commonSettings: _*
  )
  .settings(
    publish / skip := true,
    publishArtifact := false,
    moduleName := "nspl-docs-js"
  )
  .enablePlugins(ScalaJSPlugin)

lazy val docs = project
  .in(file("nspl-docs"))
  .dependsOn(core, saddle, awt)
  .settings(
    commonSettings: _*
  )
  .settings(
    crossScalaVersions := List("3.3.5"),
    Compile / doc / sources := Seq.empty
  )
  .settings(
    ScalaUnidoc / unidoc / unidocProjectFilter :=
      inProjects(core, awt, saddle, sharedJs, sharedJvm, canvas),
    // (inAnyProject -- inProjects(
    //   coreJS,
    //   saddleJS,
    //   scalatagsJs,
    // )),
    publish / skip := true,
    publishArtifact := false,
    moduleName := "nspl-docs",
    mdocVariables := Map(
      "VERSION" -> (awt / versionPolicyPreviousVersions).value.headOption
        .getOrElse(version.value)
    ),
    mdocJS := Some(jsdocs),
    ScalaUnidoc / unidoc / target := (LocalRootProject / baseDirectory).value / "website" / "static" / "api",
    cleanFiles += (ScalaUnidoc / unidoc / target).value
  )
  .enablePlugins(MdocPlugin, ScalaUnidocPlugin)

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(publish / skip := true)
  .aggregate(
    saddle,
    saddleJS,
    awt,
    canvas,
    sharedJs,
    sharedJvm,
    core,
    coreJS
  )
