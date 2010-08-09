\name{Math-methods}

\docType{methods}

\alias{Math-methods}

\title{Mathematical functions}

\description{
  Standard mathematical functions that can be used with a RasterLayer object as argument:   
  \code{"abs", "sign", "sqrt", "ceiling", "floor", "trunc", "log", "log10", "log2", "log1p", "acos", "acosh", "asin",
  "asinh", "atan", "atanh", "exp", "expm1", "cos", "cosh", "sin", "sinh", "tan", "tanh"}. 
}

\section{Note}{
 If the values of the output RasterLayer cannot be held in memory, they will be saved to a temporary file. 
 If you want to set the filename (and perhaps format and datatype), use \code{\link[raster]{calc}} instead of the Math-methods.
}

\seealso{ \code{\link[raster]{Arith-methods}}, \code{\link[raster]{overlay}}, \code{\link[raster]{calc}} }


\author{Robert J. Hijmans}

\examples{
r1 <- raster()
r1 <- (setValues(r1, runif(ncell(r1))) - 0.5 ) * 10
r2 <- abs(r1)
r3 <- sqrt(r1)
}

\keyword{methods}
\keyword{math}
