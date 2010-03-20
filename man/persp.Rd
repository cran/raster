\name{persp}

\docType{methods}

\alias{persp}
\alias{persp,RasterLayer-method}
\alias{persp,RasterStackBrick-method}

\title{Perspective plot}

\description{
Perspective plot of a RasterLayer. This is an implementation of a generic function in the graphics package.
}

\usage{
persp(x, ...) 
}

\arguments{
  \item{x}{A Raster* object}
  \item{...}{Any argument that can be passed to \code{\link[graphics]{persp}} (graphics package)}  
}


\section{Methods}{
\describe{

\code{persp(x, y=1, z=NULL, ...)}

\tabular{rll}{
\tab \code{x} \tab a Raster* object \cr
\tab \code{y} \tab a index of x = RasterStack \cr
\tab \code{z} \tab values of z are ignored \cr
\tab \code{...}  \tab   \cr
}
}}

\seealso{ \code{\link[graphics]{persp}}, \code{\link[raster]{contour}}, \code{\link[raster]{plot}} }

\author{ Robert J. Hijmans }

\examples{ 
r <- raster(system.file("external/test.grd", package="raster"))
persp(r)
}

\keyword{methods}
\keyword{spatial}

