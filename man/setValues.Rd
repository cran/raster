\name{setValues}

\alias{setValues}
\alias{setValues,RasterLayer-method}
\alias{setValues,RasterBrick-method}
\alias{clearValues}

\title{Set values}

\description{
You can use the setValues function to assign values to a RasterLayer or RasterBrick object. While you can access the 'values' slot of the objects directly, you would do that at your own peril because when setting values, multiple slots need to be changed; which is what these functions do. 
}

\usage{
setValues(object, values, layer=-1) 
clearValues(object)
}

\arguments{
  \item{object}{A \code{RasterLayer} or \code{RasterBrick} object }
  \item{values}{Data values to associate with the RasterLayer object. There should be values for all cells or for a single row}
  \item{layer}{Layer number (only relevant for RasterStack and RasterBrick objects) }
}


\seealso{ \code{\link[raster]{replacement}} }


\value{
a Raster* object
}
\author{ Robert J. Hijmans }

\examples{ 
r <- raster(ncol=10, nrow=10)
vals <- 1:ncell(r)
r <- setValues(r, vals)
# equivalent to
r[] <- vals
}


\keyword{ spatial }
\keyword{ methods }

