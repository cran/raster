\name{calc}

\docType{methods}

\alias{calc}
\alias{calc,RasterLayer,function-method}
\alias{calc,RasterStackBrick,function-method}
\alias{calc,RasterStack,list-method}

\title{Calculate}

\description{
 Calculate values for a new RasterLayer, from a single RasterLayer or from a RasterStack. 
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
if \code{x = "RasterLayer"}, \code{fun} should be a function that can take a single value as input, and returns a single value (e.g. \code{sqrt}). 
if \code{x = "RasterStack"}, \code{fun} should be a function that takes a vector of values as input, and returns a single value (e.g. \code{sum}).

See (\code{\link[raster]{overlay}}) to use functions that refer to specific layers, like (\code{function(a,b,c){a + sqrt(b) / c}})
}


\section{Methods}{
\describe{
 
\bold{1) \code{x} is a RasterLayer object; \code{fun} is a function}

\code{calc(x, fun, filename='', ...)}
  
\tabular{rll}{  
  \tab  \code{x} \tab a RasterLayer object \cr
  \tab  \code{fun}  \tab The function to be applied\cr
  \tab  \code{filename}  \tab Output filename (can be absent for RasterLayers that can be stored in memory) \cr
  \tab  \code{...} \tab Additional arguments. See below \cr  
}


\bold{2) \code{x} is a RasterStack object; \code{fun} is a function}

\code{calc(x, fun, filename='',...)}
  
\tabular{rll}{  
  \tab  \code{x} \tab a RasterStack object \cr
}
Other items as above

\bold{3) \code{x} is a RasterStack object; \code{fun} is a list of functions}

\code{calc(x, fun, filename='', ...)}
  
\tabular{rll}{  
  \tab  \code{x} \tab a RasterStack object \cr
  \tab  \code{fun}  \tab a list of the functions to be applied \cr
}
Other items as above



The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{progress} \tab Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""  \cr
 }

}}


\value{
a RasterLayer object
}


\seealso{ \code{ \link[raster]{overlay}} ,  \code{ \link[raster]{reclass}},  \link[raster]{Arith-methods}, \link[raster]{Math-methods}} 


\author{ Robert J. Hijmans}

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

r2 <- r * 2
s <- stack(r, r2)
rs <- calc(s, sum)

}

\keyword{methods}
\keyword{spatial}
