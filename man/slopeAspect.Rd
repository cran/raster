\name{Slope and aspect}

\alias{slopeAspect}

\title{Slope and aspect}

\description{
Compute slope and/or aspect from elevation data. Elevation data should be in map units (typically meter) for projected (planar) raster data. They should be in meters when the coordinate reference system (CRS) is longitude/latitude. 
}

\usage{
slopeAspect(dem, filename='', out=c('slope', 'aspect'), unit='radians', neighbors=8, flatAspect, ...)  
}

\arguments{
  \item{dem}{RasterLayer object with elevation values in map units, or in meters when the crs is longitude/latitude}
  \item{filename}{Character. Filename. optional}
  \item{out}{Character vector containing one or more of these options: 'slope', 'aspect'}
  \item{unit}{Character. 'degrees' or 'radians' }
  \item{neighbors}{Integer. Indicating how many neighboring cells to use to compute slope for any cell. Either 8 (queen case) or 4 (rook case), see Details}
  \item{flatAspect}{Numeric or NA. What value to use for aspect when slope is zero (and hence the aspect is undefined)? The default value is 90 degrees (or 0.5*pi radians) }
  \item{...}{Standard additional arguments for writing RasterLayer files}
}

\details{
When \code{neighbors=4}, slope and aspect are computed according to Fleming and Hoffer (1979) and Ritter (1987). When \code{neigbors=8}, slope and aspect are computed according to Horn (1981). The Horn algorithm may be best for rough surfaces, and the Fleming and Hoffer algorithm may be better for smoother surfaces (Jones, 1997; Burrough and McDonnell, 1998). 
}

\seealso{ \code{\link{hillShade}} }


\references{
Burrough, P., and R.A. McDonnell, 1998. Principles of Geographical Information Systems. Oxford University Press.

Fleming, M.D. and Hoffer, R.M., 1979. Machine processing of landsat MSS data and DMA topographic data for forest cover type mapping. LARS Technical Report 062879. Laboratory for Applications of Remote Sensing, Purdue University, West Lafayette, Indiana.

Horn, B.K.P., 1981. Hill shading and the reflectance map. Proceedings of the IEEE 69(1):14-47

Jones, K.H., 1998. A comparison of algorithms used to compute hill slope as a property of the DEM. Computers & Geosciences 24(4): 315-323 

Ritter, P., 1987. A vector-based slope and aspect generation algorithm. Photogrammetric Engineering and Remote Sensing 53 (8):1109-1111
}

\author{Robert J. Hijmans}

\keyword{spatial}
