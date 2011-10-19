\name{initialize}

\alias{init}

\title{Intitialize}

\description{
Set values for a new raster (from scratch), either using a formula or by choose a variable \code{v} from 'x', 'y', 'col', 'row', or 'cell'.
When using a function, cell values are initialized without reference to pre-existing values. E.g., initialize with a random number (\code{fun=runif}).
Either supply an argument to \code{fun}, or to \code{v}, but not both.
}

\usage{
init(raster, fun, v, filename="", ...) 
}

\arguments{
  \item{raster}{A RasterLayer object}
  \item{fun}{The function to be applied. This must be a function that can take the number of cells as a single argument to return a vector of values with a length equal to the number of cells}
  \item{v}{'x', 'y', 'row', 'col', or 'cell'}
  \item{filename}{Output filename}
  \item{...}{Additional arguments. See Details}  
}

\details{
The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{progress} \tab Character. "text", "window", or "" (the default, no progress bar)  \cr
 }
 
}

\value{
A new RasterLayer object, and, in some cases, the side effect of a new file on disk.
}

\author{Robert J. Hijmans}

\examples{
r <- raster(ncols=36, nrows=18)
r <- init(r, fun=runif)
# there are different ways to set all values to 1 
set1f <- function(x){rep(1, x)}
r <- init(r, fun=set1f, filename='test.grd', overwrite=TRUE)
# easier to do "manually"
r <- setValues(r, rep(1, ncell(r)))
# or  
r[] <- rep(1, ncell(r))
r[] <- 1
}

\keyword{spatial}

