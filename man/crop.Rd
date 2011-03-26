\name{crop}

\alias{crop}
\alias{crop,Raster-method}

\title{Crop}

\description{
crop returns a geographic subset of the input Raster* object as specified by an Extent object (or object from which an extent object can be extracted/created). Areas in the bounding box outside of the intput RasterLayer are ignored, and the Extent is aligned to the cells of the input RasterLayer. 
}

\usage{
crop(x, y, ...) 
}

\arguments{
  \item{x}{A Raster* object}
  \item{y}{Extent object, or any object from which an Extent object can be extracted (see below)}
  \item{...}{additional arguments. See Details.}  
}

\details{
Objects that have a Extent object include RasterLayer, RasterStack and RasterBrick. They can also be extracted from objects descending from the Spatial class from the sp package. You can check this with the \code{\link[raster]{extent}} function. New Extent objects can be also be created with \code{\link{extent}} and with \code{\link{drawExtent}} by clicking twice on a plot.
 
The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
 \tab \code{filename}  \tab Output filename (can be absent for RasterLayers that can be stored in memory) \cr
 \tab \code{format}  \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
 \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
 \tab \code{progress}  \tab Character. "text", "window", or "" (the default, no progress bar)  \cr
} 
}

\value{
A new Raster* object
}

\author{Robert J. Hijmans}

\seealso{ 
\code{\link[raster]{expand}}, \code{\link[raster]{merge}}
}

\examples{
r <- raster(nrow=45, ncol=90)
r[] <- 1:ncell(r)
e <- extent(-160, 10, 30, 60)
rc <- crop(r, e)	
}

\keyword{spatial}
