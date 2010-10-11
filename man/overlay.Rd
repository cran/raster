\name{overlay}

\docType{methods}

\alias{overlay}
\alias{overlay,RasterLayer,RasterLayer-method}
\alias{overlay,RasterStackBrick,RasterStackBrick-method}
\alias{overlay,RasterLayer,missing-method}
\alias{overlay,RasterStackBrick,missing-method}
\alias{overlay,list,missing-method}

\title{Overlay Raster objects}

\description{
Create a new Raster* object, based on two or more existing and Raster* objects. 

You should supply a function \code{fun} to set the way that the RasterLayers are combined. The number of arguments in the function must match the number of Raster objects. For example, if you combine two RasterLayers you could use multiply: \code{fun=function(x,y){return(x*y)}} percentage: \code{fun=function(x,y){return(100 * x / y)}}. If you combine three layers you could use \code{fun=function(x,y,z){return((x + y) * z)}}

If a single mutli-layer object is provided, its layers are treated as individual RasterLayers. If multiple multi-layer objects are provided, they should have the same number of layers, and a brick with the same number of layers is returned (objects could also have a single layer, which is recycled).

You can also supply a function such as \code{sum}, that takes \code{n} arguments. Note that the function must work for vectors (not only single numbers)
}

\section{Methods}{
\describe{
Formal definition of the overlay method (in the sp package)

\code{overlay(x, y, ...)}

\bold{1) \code{x} and \code{y} are Raster* objects}

\code{overlay(x, y, ..., fun, datatype, format, overwrite, progress)}

\tabular{rll}{
\tab \code{x} \tab Raster* object \cr
\tab \code{y} \tab Raster* object \cr
\tab \code{...} \tab Additional Raster* objects \cr
\tab \code{fun} \tab the function to be applied. The number of arguments of the function should match the number of Raster objects, or it should take any number of arguments  \cr
}
  
\bold{2) \code{x} is a RasterStack or RasterBrick object, \code{y} is missing}

In this case, the function returns a single RasterLayer based on computations that combine the individual layers of the Raster* object. 

\code{overlay(x, fun, overwrite, format, datatype, progress)}
\tabular{rll}{
\tab \code{x}  \tab RasterStack or RasterBrick object \cr
\tab \code{fun} \tab Function to be applied. The number of arguments of the function should match the number of layers of the RasterStack/Brick object  \cr
}


\bold{3) \code{x} is a RasterLayer object, \code{y} is missing }

This is equivalent to using the \code{\link[raster]{calc}} function 

\code{overlay(x, fun, overwrite, format, datatype, progress)}
\tabular{rll}{
\tab \code{x}  \tab RasterLayer object \cr
\tab \code{fun}  \tab Function to be applied. This should be a function that takes a single argument \cr
}


In all cases you can use these arguments:

\tabular{rll}{
\tab \code{filename} \tab  filename for the output raster. A valid filename must be provided when the data of the input rasters are on disk \cr
\tab \code{overwrite}\tab logical. If \code{TRUE}, existing files will be overwritten \cr
\tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
\tab \code{progress} \tab Character. "text", "window", or "" (the default, no progress bar)  \cr
}

}
}

\details{
In stead of the overlay function you can also use normal aritmethic functions such as \code{*, /, +, -} with RasterLayer objects (see examples), but then you cannot specifiy an output filename. However, the overlay function should be more efficient when using large data files that cannot be loaded into memory, as the use of the standard arithmetic functions might cause the creation of many temporary files.

While you can supply functions such as \code{sum} or \code{mean}, it would be more direct to use the Raster* objects are arguments to those functions (e.g. \code{sum(r1,r2,r3)})  

For "overlays" involving Raster objects and polyongs, lines, or points, see, e.g.:
\code{ \link[raster]{polygonsToRaster}, \link[raster]{linesToRaster}, \link[raster]{pointsToRaster}, \link[raster]{xyValues}, \link[raster]{polygonValues} }

}

\value{
Raster* object
}

\seealso{\code{ \link[raster]{calc},  \link[raster]{Arith-methods}} }

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

#use the individual layers of a RasterStack to get a RasterLayer
s <- stack(r1, r2)
r5 <- overlay(s, fun=function(x,y){return(x*y)} )

#use two RasterStack objects to get a RasterBrick
s2 <- stack(r2, r1)
b <- overlay(s, s2, fun=function(x,y){return(x*y)} )

# use a single RasterLayer (same as calc function)
r6 <- overlay(r1, fun=function(x){return(sqrt(x))} )

# multiplication with more than two layers (make sure the number of RasterLayers matches the arguments of 'fun'
r7 <- overlay(r1, r2, r3, r4, fun=function(a,b,c,d){return(a*b+c*d)} )  
# equivalent function, efficient if values can be loaded in memory
r8 <- r1 * r2 + r3 * r4
}
 
\keyword{methods}
\keyword{spatial}
