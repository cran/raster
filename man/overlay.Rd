\name{overlay}

\docType{methods}

\alias{overlay}
\alias{overlay,RasterLayer,RasterLayer-method}
\alias{overlay,RasterLayer,missing-method}
\alias{overlay,RasterStackBrick,missing-method}
\alias{overlay,list,missing-method}

\title{Overlay}

\description{
Create a new RasterLayer object, based on two or more existing and RasterLayers (possibly as part of a RasterStack or RasterBrick) of the same extent and resolution. 

You should supply a function \code{fun} to set the way that the RasterLayers are combined. The number of arguments in the function must match the number of RasterLayer objects. For example, if you combine two RasterLayers you could use multiply: \code{fun=function(x,y){return(x*y)}} percentage: \code{fun=function(x,y){return(100 * x / y)}}. If you combine three layers you could use \code{fun=function(x,y,z){return((x + y) * z)}}
You can also supply a function such as \code{sum}, that takes \code{n} arguments. Note that the function must work when its arguments are vectors (not only single numbers)
}

\section{Methods}{
\describe{
Formal definition of the overlay method (in the sp package)

\code{overlay(x, y, ...)}

\bold{1) x and y are Raster* objects}

\code{overlay(x, y, ..., fun, datatype, format, overwrite, progress)}

\tabular{rll}{
\tab \code{x} \tab a RasterLayer object \cr
\tab \code{y} \tab a RasterLayer object \cr
\tab \code{...} \tab additional RasterLayer objects \cr
\tab \code{fun} \tab the function to be applied. This should be a function that takes two numbers as an argument  \cr
\tab \code{filename} \tab  filename for the output raster. A valid filename must be provided when the data of the input rasters are on disk \cr
\tab \code{overwrite}\tab logical. If \code{TRUE}, existing files will be overwritten \cr
\tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
\tab \code{progress} \tab Character. "text", "window", or "" (the default, no progress bar)  \cr
}
  
  
\bold{2) x is a Raster* object, y is missing (this is equivalent to \code{\link[raster]{calc}})}

\code{overlay(x, fun, overwrite, format, datatype, progress)}

\tabular{rll}{
  \tab \code{x}  \tab a RasterLayer object \cr
  \tab \code{fun}  \tab the function to be appliepd. This should be a function that takes two numbers as an argument \cr
  \tab \code{filename}  \tab  filename for the output raster. A valid filename must be provided when the data of the input rasters are on disk \cr
  \tab \code{overwrite}  \tab logical. If \code{TRUE}, existing files will be overwritten \cr
  \tab \code{format}  \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
}
}
}

\details{
While you can supply functions such as \code{sum} or \code{mean}, it would be more direct to use the Raster* objects are arguments to those functions (e.g. \code{sum(r1,r2,r3)})  
In stead of the overlay function you can also use normal aritmethic functions such as \code{*, /, +, -} with RasterLayer objects (see examples), but then you cannot specifiy an output filename. 
However, the overlay function should be more efficient when using large data files that cannot be loaded into memory, as the use of the standard arithmetic functions might cause the creation of many temporary files.
}

\value{
a new RasterLayer object and in some cases, if a filename is provided or the output values cannot be stored in memory, 
the side effect of a file on disk
}

\seealso{ \code{\link[raster]{Arith-methods}} }

\author{
Robert J. Hijmans 
}

\examples{
r <- raster(ncol=10, nrow=10)
r1 <- init(r, fun=runif)
r2 <- init(r, fun=runif)
r3 <- overlay(r1, r2, fun=function(x,y){return(x+y)})

# long version for multiplication
r4 <- overlay(r1, r2, fun=function(x,y){return(x*y)} )

#use a stack
s <- stack(r1, r2)
r5 <- overlay(s, fun=function(x,y){return(x*y)} )

# use a single RasterLayer (same as calc function)
r6 <- overlay(r1, fun=function(x){return(sqrt(x))} )

# multiplication with more than two layers (make sure the number of RasterLayers matches the arguments of 'fun'
r7 <- overlay(r1, r2, r3, r4, fun=function(a,b,c,d){return(a*b+c*d)} )  
# equivalent function, efficient if values can be loaded in memory
r8 <- r1 * r2 + r3 * r4
}
 
\keyword{methods}
\keyword{spatial}
