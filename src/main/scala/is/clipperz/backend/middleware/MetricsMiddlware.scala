package is.clipperz.backend.middleware

// import java.nio.file.{ Files, Path }
import zio.nio.file.{ Files, Path }
import java.util.concurrent.TimeUnit.{ SECONDS, NANOSECONDS }

import scala.jdk.CollectionConverters.*

import zio.{ Clock, Duration, Chunk, RuntimeFlags, Schedule, Task, Trace, RIO, ZIO, durationInt }
import zio.metrics.{ Metric, MetricLabel, MetricKeyType }
import zio.http.{ Handler, HandlerAspect, Method, Middleware, RoutePattern, Response, Request, Routes }
import zio.metrics.MetricKeyType.Histogram.Boundaries
import java.time.temporal.ChronoUnit
import zio.telemetry.opentelemetry.tracing.Tracing
import io.opentelemetry.api.common.Attributes

def collectFileSystemMetrics (path: Path): RIO[Tracing, (Double, Double, Array[Double])] =
    ZIO.serviceWithZIO[Tracing](_.span("file system metrics computation", attributes = Attributes.builder().put("archive", path.filename.toFile.toString).build()){
        Files.walk(path)
            // .map(_.toFile().nn)
            // .filter(file => file.isFile() && !file.isHidden())
            .filterZIO(path => Files.isRegularFile(path).zip(Files.isHidden(path)).map((regular, hidden) => (regular && !hidden)))
            // .map(file => (1, file.length()))
            .mapZIO(path => Files.size(path).map(size => (1, size)))
            //  count, totalSize, [size]
            .runFold((0.0, 0.0, Array.empty[Double]))((acc, tuple) => ((acc._1 + tuple._1), (acc._2 + tuple._2), acc._3 :+ (tuple._2.toDouble)))
            @@ Metric.gauge("files.count")
                .contramap[(Double, Double, Array[Double])](_._1)
                // .tagged("archive", path.getFileName().nn.toString())
                .tagged("archive", path.filename.toFile.toString)
            @@ Metric.gauge("files.size")
                .contramap[(Double, Double, Array[Double])](_._2/1000)
                .tagged("archive", path.filename.toFile.toString)
    })

def scheduledFileSystemMetricsCollection (path: Path) =
    collectFileSystemMetrics(path) `repeat` Schedule.fixed(30.minutes)