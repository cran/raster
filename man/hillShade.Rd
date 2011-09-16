\name{hillShade}

\alias{hillShade}

\title{Hill shading}

\description{
Compute hill shade from slope and aspect layers (both in radians). Slope and aspect can be computed with \code{\link{slopeAspect}}. 

A hill shade layer is often used as a backdrop on top of which another, semi-transparant, layer is drawn.
}

\usage{
hillShade(slope, aspect, angle=45, direction=0, filename='', ...)  
}

\arguments{
  \item{slope}{RasterLayer object with slope values (in radians) }
  \item{aspect}{RasterLayer object with aspect values (in radians) }
  \item{angle}{ The the elevation angle of the light source (sun), in degrees}
  \item{direction}{ The direction (azimuth) angle of the light source (sun), in degrees}
  \item{filename}{Character. Optional filename}
  \item{...}{Standard additional arguments for writing RasterLayer files}
}


\seealso{ \code{\link{slopeAspect}} }

\author{Andrew Bevan, Robert J. Hijmans}


\references{
Horn, B.K.P., 1981. Hill shading and the reflectance map. Proceedings of the IEEE 69(1):14-47
}


\examples{
\dontrun{
dem <- getData('alt', country='CHE')
slas <- slopeAspect(dem)
hill <- hillShade(slas[[1]], slas[[2]], 40, 270)
plot(hill, col=grey(0:100/100), legend=FALSE, main='Switzerland')
plot(dem, col=rainbow(25, alpha=0.35), add=TRUE)
}
}

\keyword{spatial}

