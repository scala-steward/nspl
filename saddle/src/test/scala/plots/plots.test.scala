import org.nspl._
import org.nspl.saddle._
import org.nspl.data._
import org.nspl.awtrenderer.{defaultAWTFont => _, _}
import org.saddle._
import org.saddle.order._

import org.saddle.csv._
import org.saddle.scalar.ScalarTagDouble
import org.saddle.index.InnerJoin

class SaddlePlotSpec extends munit.FunSuite {

  override val munitTimeout = scala.concurrent.duration.Duration(300, "s")
  implicit val myfont: FontConfiguration = font("Hasklig")

  def readFrameFromClasspath(s: String) =
    CsvParser
      .parseStringWithHeader[String](
        scala.io.Source
          .fromInputStream(
            getClass.getResourceAsStream(s)
          ).mkString,
        recordSeparator = "\n"
      )
      .toOption
      .get

  test("plot gallery") {
    {
      // val evec =
      // readFrameFromClasspath("/evec.csv")
      // .mapValues(ScalarTagDouble.parse)
      val rotated =
        readFrameFromClasspath("/rotated.csv").mapValues(str => ScalarTagDouble.parse(str.toCharArray(),0,str.length))
      val data = readFrameFromClasspath("/data.csv")
        .mapValues(str => ScalarTagDouble.parse(str.toCharArray(),0,str.length))
        .colAt(Array(0, 1, 2, 3))

      val species = readFrameFromClasspath("/data.csv").colAt(4)
      val spec2Num = species.toVec.toSeq.distinct.sorted.zipWithIndex.toMap
      val spec: Series[Int, Double] =
        species.mapValues(spec2Num).mapValues(_.toDouble)

      val eval = readFrameFromClasspath("/sqrteigen.csv")
        .mapValues(str => ScalarTagDouble.parse(str.toCharArray(),0,str.length))
        .mapValues(x => x * x)

      val data2 = data.addCol(spec, "spec", InnerJoin)

      val scree = xyplot(
        indexed(eval.firstCol("x").toVec.toSeq.sorted.reverse) -> line()
      )(
        par
          .withXAxisMargin(0)
          .withXLab("Order")
          .withYLab("Eval")
          .withMain("Scree")
      )

      val hist1 = xyplot(
        HistogramData(rotated.firstCol("PC1").toVec.toSeq, 10) -> bar()
      )(
        par
          .withXLab("PC1")
          .withYLab("freq.")
          .withMain("Loading distribution")
          .withYLim(Some((0d, Double.NaN)))
      )

      val bar1 = barplotVertical(
        Series("a" -> (-2d), "b" -> (-1d), "c" -> 0d, "d" -> 1d, "e" -> 2d),
        color = RedBlue(-2, 2)
      )(par)
      val bar2 = barplotHorizontal(
        Series("a" -> (-2d), "b" -> (-1d), "c" -> 0d, "d" -> 1d, "e" -> 2d)
      )(par)

      val density1 = xyplot(
        density(rotated.firstCol("PC1").toVec.toSeq) -> line(
          stroke = StrokeConf(1 fts)
        )
      )(par.xlab("PC1").ylab("dens.").main("Loading distribution"))

      val r1 = rasterplotFromFrame(rotated)(par.yLabFontSize(0.1 fts))

      val fig0 = xyplot(
        data2.col("Sepal.Length", "Sepal.Width", "spec")
      )(
        par
          .extraLegend(
            spec2Num.toSeq.map(x =>
              x._1 -> PointLegend(
                shape = Shape.rectangle(0, 0, 1, 1),
                color = DiscreteColors(spec2Num.size)(x._2)
              )
            )
          )
          .xlab("Sepal.Length")
          .ylab("Sepal.Width")
          .main("Iris data")
      )

      val fig1 = xyplot(
        Frame(
          (rotated.col("PC1", "PC2").toColSeq :+ ("spec" -> spec)): _*
        ) -> point(
          valueText = true
        ),
        density2d(
          rotated.firstCol("PC1").toVec.toSeq zip rotated
            .firstCol("PC2")
            .toVec
            .toSeq
        )
      )(
        par
          .extraLegend(
            spec2Num.toSeq.map(x =>
              x._1 -> PointLegend(
                shape = Shape.rectangle(0, 0, 1, 1),
                color = DiscreteColors(spec2Num.size)(x._2)
              )
            )
          )
          .xlab("PC1")
          .ylab("PC2")
          .main("PC1 vs PC2")
      )

      // println(pdfToFile(fig1.build))
      // ???
      val densmap = rasterplot(
        densityMatrix(
          rotated.firstCol("PC1").toVec.toSeq zip rotated
            .firstCol("PC2")
            .toVec
            .toSeq
        )
      )(par)

      val fig2 = xyplot(
        Frame((rotated.col("PC2", "PC3").toColSeq :+ ("spec" -> spec)): _*)
      )(
        par
          .extraLegend(
            spec2Num.toSeq.map(x =>
              x._1 -> PointLegend(
                shape = Shape.rectangle(0, 0, 1, 1),
                color = DiscreteColors(spec2Num.size)(x._2)
              )
            )
          )
          .xlab("PC2")
          .ylab("PC3")
          .main("PC2 vs PC3")
      )

      val pc3 = rotated.firstCol("PC3").toVec.toSeq
      val pc2 = rotated.firstCol("PC2").toVec.toSeq

      val fig3 = binnedboxplot(pc3, pc2)(par.xlab("PC2").ylab("PC3"))

      val fig4 = boxplot(
        data2
          .firstCol("Sepal.Length")
          .toVec
          .toSeq -> data2.firstCol("Sepal.Width").toVec.toSeq,
        xnames = Seq("Sepal Length", "Sepal Width")
      )(par.ylab("Sepal Length").xLabelRotation(-0.3))

      val boxpl2 =
        boxplotFromLabels(List("a" -> 0d, "a" -> 1d, "b" -> 3d, "b" -> 4d))(
          par
        )

      val contour = contourplot(
        xlim = -2d -> 2d,
        ylim = -2d -> 2d,
        f = (x, y) => (x * x + y * y - 0.5 + x * y),
        n = 50,
        levels = 10
      )(par)

      val density2 = xyplot(
        density(rotated.firstCol("PC1").toVec.toSeq) -> line()
      )(par.xlab("PC1").ylab("dens.").main("Loading distribution"))

      val empty2 =
        xyplotarea(
          id = new PlotId,
          Nil,
          AxisSettings.simple(LinearAxisFactory),
          AxisSettings.simple(LinearAxisFactory),
          origin = Some(Point(0.0, 0.0)),
          xlim = Some(-1d -> 1d),
          ylim = Some(-1d -> 1d)
        )

      val rs =
        (1 to 99 map (_ => scala.util.Random.nextGaussian())).toSeq :+ 1e3

      val p6 = rasterplot(
        rasterFromSeq(rs, 10, 10),
        valueText = true,
        valueColor = Color.white,
        valueFontSize = 0.3 fts
      )(
        par
          .xLabFontSize(0.5 fts)
          .yLabFontSize(0.1 fts)
          .xnames(Seq(0.0 -> "sdfsf", 1.0 -> "dsfds adfdf adfs f"))
          .ynames(Seq(0.0 -> "dsfsdf", 2.0 -> "dfsdf asdfdf asdfds sdfadsf"))
          .main("1main 2main main3 m4ain m5ain m6ain 7mian 8mian 9main ")
          .mainFontSize(1 fts)
      )

      val p6b = rasterplot(
        rasterFromSeq(rs, 10, 10),
        valueText = true,
        colormap = LogHeatMapColors(),
        valueColor = Color.white,
        valueFontSize = 0.3 fts
      )(
        par
          .xLabFontSize(0.5 fts)
          .yLabFontSize(0.1 fts)
          .xnames(Seq(0.0 -> "sdfsf", 1.0 -> "dsfds adfdf adfs f"))
          .ynames(Seq(0.0 -> "dsfsdf", 2.0 -> "dfsdf asdfdf asdfds sdfadsf"))
          .main("1main 2main main3 m4ain m5ain m6ain 7mian 8mian 9main ")
          .mainFontSize(1 fts)
      )

      val barplot2 = {
        val dataraw: IndexedSeq[(Double, Double, Double, Double)] =
          1 to 100 map (i =>
            (
              i.toDouble,
              scala.util.Random.nextInt(i).toDouble,
              scala.util.Random.nextInt(101 - i).toDouble,
              scala.util.Random.nextInt(50).toDouble
            )
          )

        stackedBarPlot(
          dataraw,
          List(
            (1, "red", Color.red),
            (2, "blue", Color.blue),
            (3, "green", Color.green)
          ),
          relative = true
        )(par)
      }

      val colortest = xyplot(
        indexed(1 to 60 map (_.toDouble) toSeq) -> point(
          color = DiscreteColors(60),
          colorCol = 1
        )
      )()
      val ticktest = xyplot(
        indexed(
          Seq(0.001, 0.01, 0.0, -1.2, 1.1, 100.0, 100.001, 100.1, 1000d, 10000d)
        ) -> point()
      )()

      val ticktest2 =
        xyplot(indexed(Seq(100.0, 100.001, 100.1, 100.101234)) -> point())()
      val ticktest3 = xyplot(
        indexed(Seq(1e-5, 0.00003, 0.00002, 0.00001, 2.313e-6)) -> point()
      )()
      val ticktest4 = xyplot(
        indexed(Seq(1e-5, 0.00003, 0.00002, 0.00001, 2.313e-6, 1e6)) -> point()
      )()
      val ticktest5 = xyplot(indexed(Seq(1e6, 1.02e6, 2.567e7)) -> point())()
      val ticktest6 =
        xyplot(indexed(Seq(100.0, 100.001, 100.0015, 100.0016)) -> point())()

      val logaxistest =
        xyplot(
          Seq(
            1e-6 -> 1e-6,
            1e-5 -> 1e-5,
            1e-4 -> 1e-4,
            1e-3 -> 1e-3,
            1e-2 -> 1e-2,
            1e-1 -> 1e-1,
            1d -> 1d,
            10d -> 10d,
            1e2 -> 100d,
            1e3 -> 1000d,
            1e4 -> 10000d,
            1e5 -> 1e5,
            1e6 -> 1e6,
            1e7 -> 1e7
          ) -> point()
        )(par.ylog(true).xlog(true))
      val logaxistest2 =
        xyplot(
          Seq(1.123e-6 -> 1e-6, 1e-5 -> 1e-5, 1e-4 -> 3.132e-4) -> point()
        )(par.ylog(true).xlog(true))
      val logaxistest3 =
        xyplot(
          Seq(0.92 -> 1d, 10d -> 10d, 1e2 -> 100d, 1e3 -> 1324d) -> point()
        )(par.ylog(true).xlog(true))
      val logaxistest4 =
        xyplot(
          Seq(0.92 -> 1d, 10d -> 10d, 1e2 -> 100d, 1e3 -> 1324d) -> point()
        )(par.ylog(true).xlog(true).xlim(Some(0.5 -> 1500d)))

      val hexbinTest = {
        val binning =
          hexbin(
            rotated.firstCol("PC1").toVec.toSeq zip rotated
              .firstCol("PC2")
              .toVec
              .toSeq,
            size = 0.2,
            color = GrayScale(0, 30)
          )
        xyplot(
          binning,
          Frame(
            (rotated.col("PC1", "PC2").toColSeq :+ ("spec" -> spec)): _*
          ) -> point(
            size = 1
          )
        )(
          par
            .extraLegend(
              spec2Num.toSeq.map(x =>
                x._1 -> PointLegend(
                  shape = Shape.rectangle(0, 0, 1, 1),
                  color = DiscreteColors(spec2Num.size)(x._2)
                )
              )
            )
            .xlab("PC1")
            .ylab("PC2")
            main ("PC1 vs PC2")
        )
      }

      val rotations = fitToBounds(
        sequence(
          List(
            ShapeElem(Shape.circle(0.01d)).translate(0, 0),
            ShapeElem(Shape.circle(0.01d)).translate(0, 1),
            ShapeElem(Shape.circle(0.01d)).translate(1, 1),
            ShapeElem(Shape.circle(0.01d)).translate(1, 0),
            ShapeElem(Shape.circle(0.01d)).translate(0, 1).rotate(0.1),
            ShapeElem(Shape.circle(0.01d)).translate(0, 1).rotate(0.2),
            ShapeElem(Shape.circle(0.01d)).translate(0, 1).rotate(0.3),
            ShapeElem(Shape.circle(0.01d)).translate(0, 1).rotate(0.4),
            ShapeElem(Shape.circle(0.01d)).translate(0, 1).rotate(0.5),
            ShapeElem(Shape.circle(0.01d)).translate(0, 1).rotate(0.6),
            ShapeElem(Shape.circle(0.01d)).translate(0, 1).rotate(0.7),
            ShapeElem(Shape.circle(0.01d)).translate(0, 1).rotate(0.8),
            ShapeElem(Shape.circle(0.01d)).translate(0, 1).rotate(0.9),
            ShapeElem(Shape.circle(0.01d)).translate(0, 1).rotate(1),
            ShapeElem(Shape.circle(0.01d)).translate(0, 1).rotate(0.1, 0, 0.75),
            ShapeElem(Shape.circle(0.01d)).translate(0, 1).rotate(0.2, 0, 0.75),
            ShapeElem(Shape.circle(0.01d)).translate(0, 1).rotate(0.3, 0, 0.75),
            ShapeElem(Shape.circle(0.01d)).translate(0, 1).rotate(0.4, 0, 0.75),
            ShapeElem(Shape.circle(0.01d)).translate(0, 1).rotate(0.5, 0, 0.75),
            ShapeElem(Shape.circle(0.01d)).translate(0, 1).rotate(0.6, 0, 0.75),
            ShapeElem(Shape.circle(0.01d)).translate(0, 1).rotate(0.7, 0, 0.75),
            ShapeElem(Shape.circle(0.01d)).translate(0, 1).rotate(0.8, 0, 0.75),
            ShapeElem(Shape.circle(0.01d)).translate(0, 1).rotate(0.9, 0, 0.75),
            ShapeElem(Shape.circle(0.01d)).translate(0, 1).rotate(1, 0, 0.75)
          ),
          FreeLayout
        ),
        Bounds(0, 0, 100, 100)
      )

       val overlay = {
        val s1 = ShapeElem(Shape.circle(100))
        val s2 = ShapeElem(Shape.circle(200)).transform(AffineTransform.translate(100d,0d))
        group(s1,s2,ZStack)
      }

      val gallery = group(
        group(
          scree,
          density2,
          rotations,
          ticktest,
          ticktest2,
          ticktest3,
          ticktest4,
          ticktest5,
          ticktest6,
          logaxistest,
          logaxistest2,
          logaxistest3,
          logaxistest4,
          TableLayout(3)
        ),
        bar1,
        bar2,
        boxpl2,
        colortest,
        p6,
        p6b,
        empty2,
        xyplot(Seq(0d -> 0d, 1d -> 1d, 2d -> 2d))(par),
        r1,
        hist1,
        contour,
        density1,
        fig0,
        fig1,
        hexbinTest,
        densmap,
        fig2,
        fig3,
        fig4,
        barplot2,
        group(
          overlay,
          barplot2,
          TextBox(
            "aabcd fafafafabcd fafafafabcd faafafafabcd fafafafabcafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd ffafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafbcd fafafaf"
          ),
          TableLayout(2)
        ),
        ColumnLayout(4)
      )

     

      val text = fitToWidth(
        group(
          
          ShapeElem(Shape.circle(1)),
          TextBox("ABC abc def ghijklmn opqrstvuwxyz"),
          TextBox("abc def ghijklmn opqrstvuwxyz", width = Some(30d))
            .translate(10, 30),
          TextBox("abc def ghijklmn opqrstvuwxyz", width = Some(30d))
            .translate(10, 30)
            .rotate(math.Pi / 2, 0d, 0d),
          TextBox("abc def ghijklmn", fontSize = 0.1 fts).rotate(1d),
          TextBox("opqrstvuwxyz", fontSize = 0.1 fts).translate(10, 30),
          TextBox("abc def ghijklmn opqrstvuwxyz", fontSize = 1 fts)
            .translate(100, 30),
          FreeLayout
        ),
        200
      )

      {
        import awtrenderer._

        // show(gallery)
        println(pngToFile(gallery.build, width = 10000))
        println(pdfToFile(gallery.build))
        println(pdfToFile(text))
        while (false) {
          gallery.build
        }
        // println(pdfToFile(text))
        // println(renderToFile(gallery, 1000, "image/svg"))
      }
      {
        // import scalatagrenderer._
        // println(svgToFile(gallery))
      }
    }

  }
}
