\name{Arith-methods}

\docType{methods}

\alias{Arith-methods}

\title{Arithmic with Raster* objects}

\description{
  Standard arithmic operators for computations with Raster* objects and numeric values. The following operators are available:
\code{ +, -, *, /, ^, \%\%, \%/\% }
  Operators are applied on a cell by cell basis, and single numeric values are recycled. 
  Input RasterLayers must have the same extent and resolution.
}

\section{details}{
If the values of the output Raster* cannot be held in memory, they will be saved to a temporary file. You can use \code{\link[raster]{rasterOptions}} to set the default file format, datatype and progress bar.
}


\value{
A Raster* object, and in some cases the side effect of a new file on disk.
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
b <- brick(r1, r2, r3)
b <- b * 10
}

\keyword{methods}
\keyword{math}
\keyword{spatial}
