\name{calc}

\docType{methods}

\alias{calc}
\alias{calc,RasterLayer,function-method}
\alias{calc,RasterStackBrick,function-method}

\title{Calculate}

\description{ 
Calculate values for a new Raster object from another Raster* object, using a formula. 

If \code{x} is a RasterLayer, \code{fun} should be a function that can take a single value as input, and returns a single value (e.g. \code{sqrt}). 

If \code{x} is a RasterStack or RasterBrick, and \code{fun} returns the same number of values as the input vector (e.g., \code{function(x){ x * 10}}) it, calc returns a RasterBrick. But calc returns a RasterLayer if \code{fun} takes a vector of values and \code{na.rm} (or \code{(...)} as input, and return a single value (e.g. \code{sum}. 

 
In many cases, what can be achieved with \code{calc} in a more intuitive 'raster-algebra' notation (see \code{\link[raster]{Arith-methods}}). For example, \code{r <- r * 2} rather than \code{r <- calc(r, fun=function(x){x * 2}}. However, \code{calc} should be faster when using complex formulas on large datasets. With \code{calc} it is also easier to set output file preferences if applicable. 
 
See (\code{\link[raster]{overlay}}) to use functions that refer to specific layers, like (\code{function(a,b,c){a + sqrt(b) / c}})
}

\usage{
calc(x, fun, ...) 
}

\arguments{
  \item{x}{a Raster* object}
  \item{fun}{a function}
  \item{...}{Additional arguments. See Details}  
}

\details{
The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{na.rm}  \tab Remove NA values (only relevant when summarizing a mutilayer Raster object into a RasterLayer) \cr
  \tab \code{filename}  \tab Output filename (optional) \cr
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{progress} \tab Character. "text", "window", or "" (the default, no progress bar)  \cr
}
}

\value{
a Raster* object
}


\seealso{ \code{ \link[raster]{overlay}} ,  \code{ \link[raster]{reclass}},  \link[raster]{Arith-methods}, \link[raster]{Math-methods}} 


\author{ Robert J. Hijmans and Matteo Mattiuzzi}

\examples{
r <- raster(ncols=36, nrows=18)
r[] <- 1:ncell(r)

# multiply values with 10
fun <- function(x) { x * 10 }
rc <- calc(r, fun)

# set values below 100 to NA. 
fun <- function(x) { x[x<100] <- NA; return(x) }
rc <- calc(r, fun)

# set NA values to -9999
fun <- function(x) { x[is.na(x)] <- -9999; return(x)} 
rc <- calc(rc, fun)

# using a RasterStack as input
s <- stack(r, r*2, sqrt(r))
# return a RasterLayer
rs <- calc(s, sum)

# return a RasterStack
rs <- calc(s, fun=function(x){x * 10})
# return a RasterStack, using recycling by layer
rs <- calc(s, fun=function(x){x * c(1, 5, 10)})

}

\keyword{methods}
\keyword{spatial}
