\name{crosstab}

\docType{methods}

\alias{crosstab}
\alias{crosstab,RasterLayer,RasterLayer-method}
\alias{crosstab,RasterStackBrick,missing-method}

\title{Cross-tabulate}

\description{
Cross-tabulate two RasterLayer objects, or mulitiple layers in a RasterStack or RasterBrick to create a contingency table.
}

\usage{
\S4method{crosstab}{RasterLayer,RasterLayer}(x, y, digits=0, long=FALSE, progress, ...)

\S4method{crosstab}{RasterStackBrick,missing}(x, digits=0, long=FALSE, progress, ...)
}

\arguments{
  \item{x}{Raster* object}
  \item{y}{RasterLayer if \code{x} is a RasterLayer; missing if \code{x} is a RasterStack or RasterBrick}
  \item{digits}{Integer. The number of digits for rounding the values before cross-tabulation}
  \item{long}{Logical. If \code{TRUE} the results are returned in 'long' format data.frame instead of a table}
  \item{progress}{Character. "text", "window", or "" (the default, no progress bar), only for large files that cannot be processed in one step}
  \item{...}{Additional arguments that can be passed on to \code{\link[base]{table}}, such as \code{useNA="always"} to include \code{NA} values in the tabulation (the dnn argument is ignored and using it may lead to an error)}
}


\value{
A table or data.frame
}

\seealso{ \code{\link[raster]{freq}}, \code{\link[raster]{zonal}} }

\author{Robert J. Hijmans}

\examples{
r <- raster(nc=5, nr=5)
r[] <- runif(ncell(r)) * 2
s <- setValues(r, runif(ncell(r)) * 3)
crosstab(r,s)

rs <- r/s
r[1:5] <- NA
s[20:25] <- NA
x <- stack(r, s, rs)
crosstab(x, useNA='always', long=TRUE)

}

\keyword{methods}
\keyword{spatial}

