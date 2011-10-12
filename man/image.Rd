\name{image}

\docType{methods}

\alias{image}
\alias{image,RasterLayer-method}
\alias{image,RasterStackBrick-method}

\title{Image}

\description{
Create an "image" type plot of a RasterLayer. This is an implementation of a generic function in the graphics package. In most cases the 
 \code{\link[raster]{plot}} function would be preferable because it produces a legend (and has some additional options).
}

\usage{
image(x, ...) 
}

\arguments{
  \item{x}{A Raster* object}
  \item{...}{Any argument that can be passed to \code{\link[graphics]{image}} (graphics package)}  
}


\section{Methods}{
\describe{

\code{image(x, y=1, z=NULL, ...)}

\tabular{rll}{
\tab \code{x} \tab a Raster* object \cr
\tab \code{...}  \tab   \cr
}
}}

\seealso{ \code{\link[graphics]{image}}, \code{\link[raster]{contour}}, \code{\link[raster]{plot}} }

\author{ Robert J. Hijmans }

\examples{ 
r <- raster(system.file("external/test.grd", package="raster"))
image(r)
}

\keyword{methods}
\keyword{spatial}

