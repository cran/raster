\name{hillShade}

\alias{hillShade}

\title{Hill shading}

\description{
Compute hill shade from slope and aspect layers. Slope should be expressed as a tangent (fraction), and aspect layers should be in radians. Slope and aspect can be computed with \code{\link{slopeAspect}}. 

A hill shade layer is often used as a backdrop on top of which another, semi-transparant, layer is drawn.
}

\usage{
hillShade(slope, aspect, declination, direction, filename='', ...)  
}

\arguments{
  \item{slope}{RasterLayer object with slope values expressed as a fraction (tangent) }
  \item{aspect}{RasterLayer object with aspect values expressed as radians }
  \item{declination}{ The height of the light source (sun) expressed in degrees}
  \item{direction}{ The direction (azimuth) of the light source (sun) expressed in degrees}
  \item{filename}{Character. Optional filename}
  \item{...}{Standard additional arguments for writing RasterLayer files}
}


\seealso{ \code{\link{slopeAspect}} }

\author{Andrew Bevan & Robert J. Hijmans}


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

