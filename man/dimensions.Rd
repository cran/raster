\name{dim}

\alias{dim,RasterStackBrick-method}
\alias{dim,BasicRaster-method}
\alias{dim<-,BasicRaster-method}
\alias{dim<-,RasterLayer-method}
\alias{dim<-,RasterBrick-method}

\docType{methods}


\title{Dimensions of a Raster* object}

\description{
Get (or set) the number of rows, columns, and layers of a Raster* object. You cannot use this function to set the dimentions of a RasterStack object.
}

\section{Methods}{
\describe{

\code{dim(x)}

If \code{x} is a Raster* object



\code{dim(x) <- value}

\tabular{rll}{
\tab \code{x} \tab RasterLayer or RasterBrick) \cr
\tab \code{value} \tab row number, or row _and_ column number (for a RasterLayer and a RasterBrick); 

or a row and column number _and_ the number of layers (only for a RasterBrick)  \cr
}
}}

\value{
Integer
}

\seealso{ \code{ \link{ncell}, \link{extent}, \link{res} } } 

\author{ Robert J. Hijmans }

\examples{
r <- raster()
dim(r)
dim(r) <- c(18) 
dim(r)
dim(r) <- c(18, 36) 
dim(r)
b <- brick(r)
dim(b)
dim(b) <- c(10, 10, 5)
dim(b)
}

\keyword{spatial}

