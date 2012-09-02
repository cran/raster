\name{Math-methods}

\docType{methods}

\alias{Math-methods}
\alias{log,Raster-method}

\title{Mathematical functions}

\description{
Generic mathematical functions that can be used with a Raster* object as argument:   
\code{"abs", "sign", "sqrt", "ceiling", "floor", "trunc",  "cummax", "cummin", 
"cumprod", "cumsum", "log", "log10", "log2", "log1p", "acos", "acosh", "asin",
"asinh", "atan", "atanh", "exp", "expm1", "cos", "cosh", "sin", "sinh", "tan", "tanh"}. 
}


\section{Note}{
You can use the, somewhat more flexible, function \code{\link[raster]{calc}} instead of the Math-methods.
}

\seealso{ \code{\link[raster]{Arith-methods}}, \code{\link{calc}}, \code{\link{overlay}}, \code{\link{atan2}} }


\author{Robert J. Hijmans}

\examples{
r1 <- raster(nrow=10, ncol=10)
r1 <- (setValues(r1, runif(ncell(r1)))) * 10
r2 <- sqrt(r1)
s <- stack(r1, r2) - 5
b <- abs(s)
}

\keyword{spatial}
\keyword{methods}
\keyword{math}
