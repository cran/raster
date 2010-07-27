\name{Arith-methods}

\docType{methods}

\alias{Arith-methods}
\alias{Arith,numeric,RasterLayer-method}
\alias{Arith,RasterLayer,numeric-method}
\alias{Arith,RasterLayer,RasterLayer-method}

\title{Arithmic with rasters}

\description{
  Standard arithmic operators for computations with RasterLayer objects and numeric values. The following operators are available:
\code{ +, -, *, /, ^, \%\%, \%/\% }
  Operators are applied on a cell by cell basis, and single numeric values are recycled. 
  Input RasterLayers must have the same extent and resolution.
}

\section{details}{
If the values of the output RasterLayer cannot be held in memory, they will be saved to a temporary file unless a default filename has been set with \code{\link[raster]{rasterOptions}}. You can also use \code{\link[raster]{rasterOptions}} to set the format, datatype and overwrite options.
}


\value{
A RasterLayer object, and in some cases the side effect of a new file on disk.
}

\seealso{ \code{\link[raster]{Math-methods}}, \code{\link[raster]{overlay}}, \code{\link[raster]{calc}} }

\author{Robert J. Hijmans}

\examples{
r1 <- raster(ncols=10, nrows=10)
r1[] <- runif(ncell(r1))
r2 <- setValues(r1, 1:ncell(r1) / ncell(r1) )
r3 <- r1 + r2
r2 <- r1 / 10
r3 <- r1 * (r2 - 1 + r1^2 / r2)
}

\keyword{methods}
\keyword{math}
\keyword{spatial}
