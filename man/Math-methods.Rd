\name{Math-methods}

\docType{methods}

\alias{Math-methods}

\title{Mathematical functions}

\description{
  Standard mathematical functions that can be used with a Raster* object as argument:   
  \code{"abs", "sign", "sqrt", "ceiling", "floor", "trunc", "log", "log10", "log2", "log1p", "acos", "acosh", "asin",
  "asinh", "atan", "atanh", "exp", "expm1", "cos", "cosh", "sin", "sinh", "tan", "tanh"}. 
}

\section{Note}{
 If the values of the output Raster* object cannot be held in memory, they will be saved to a temporary file. 
 If you want to set the filename (and perhaps format and datatype), use \code{\link[raster]{calc}} instead of the Math-methods.
}

\seealso{ \code{\link[raster]{Arith-methods}}, \code{\link[raster]{calc}}, \code{\link[raster]{overlay}}}


\author{Robert J. Hijmans}

\examples{
r1 <- raster(nrow=10, ncol=10)
r1 <- (setValues(r1, runif(ncell(r1)))) * 10
r2 <- sqrt(r1)
s <- stack(r1, r2) - 5
b <- abs(s)
}

\keyword{methods}
\keyword{math}
