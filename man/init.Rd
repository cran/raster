\name{initialize}

\alias{init}

\title{Intitialize}

\description{
Create a new RasterLayer with values reflecting a cell property: \code{v} from 'x', 'y', 'col', 'row', or 'cell'. Alternatively, a function can be used. In that case, cell values are initialized without reference to pre-existing values. E.g., initialize with a random number (\code{fun=runif}).
Either supply an argument to \code{fun}, or to \code{v}, but not both.
}

\usage{
init(x, fun, v, filename="", ...) 
}

\arguments{
  \item{x}{A Raster* object}
  \item{fun}{The function to be applied. This must be a function that can take the number of cells as a single argument to return a vector of values with a length equal to the number of cells}
  \item{v}{'x', 'y', 'row', 'col', or 'cell'}
  \item{filename}{Output filename}
  \item{...}{Additional arguments as for \code{\link{writeRaster}}}  

}


\value{
RasterLayer
}



\examples{
r <- raster(ncols=36, nrows=18)

x <- init(r, v='cell')

y <- init(r, fun=runif)

# there are different ways to set all values to 1 
# for large rasters:
set1f <- function(x){rep(1, x)}
z1 <- init(r, fun=set1f, filename='test.grd', overwrite=TRUE)

# equivalent to
z2 <- setValues(r, rep(1, ncell(r)))
# or  
r[] <- rep(1, ncell(r))
# or  
r[] <- 1
}

\keyword{spatial}

