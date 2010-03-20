\name{contour}

\docType{methods}

\alias{contour}
\alias{contour,RasterLayer-method}
\alias{contour,RasterStackBrick-method}

\title{Contour plot}

\description{
Contour plot of a RasterLayer. This is a generic function, in this package implemented for RasterLayer objects.
}

\usage{
contour(x, ...) 
}

\arguments{
  \item{x}{A Raster* object}
  \item{...}{Any argument that can be passed to \code{\link[graphics]{contour}} (graphics package)}  
}


\section{Methods}{
\describe{

\code{contour(x, y=1, z=NULL, ...)}

\tabular{rll}{
\tab \code{x} \tab a RasterLayer object \cr
\tab \code{y} \tab a index of x = RasterStack \cr
\tab \code{z} \tab values of z are ignored \cr
\tab \code{...}  \tab  Any argument that can be passed to \code{\link[graphics]{contour}} (graphics package) \cr
}
}}

\seealso{ \code{\link[graphics]{contour}}, \code{\link[raster]{persp}}, \code{\link[raster]{plot}} }

\author{ Robert J. Hijmans }

\examples{ 
r <- raster(system.file("external/test.grd", package="raster"))
plot(r)
contour(r, add=TRUE)
}

\keyword{methods}
\keyword{spatial}

