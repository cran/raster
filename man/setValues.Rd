\name{setValues}

\alias{values<-}
\alias{values<-,RasterLayer-method}
\alias{values<-,RasterBrick-method}
\alias{values<-,RasterStack-method}

\alias{setValues}
\alias{setValues,RasterLayer-method}
\alias{setValues,RasterBrick-method}
\alias{setValues,RasterStack-method}

\title{Set values of a Raster object}

\description{
You can use the setValues function to assign values to a RasterLayer or RasterBrick object. While you can access the 'values' slot of the objects directly, you would do that at your own peril because when setting values, multiple slots need to be changed; which is what these functions do. 
}

\usage{
setValues(x, values, layer) 
values(x) <- value
}

\arguments{
  \item{x}{A \code{RasterLayer} or \code{RasterBrick} object }
  \item{values}{Cell values to associate with the RasterLayer object. There should be values for all cells}
  \item{value}{Cell values to associate with the RasterLayer object. There should be values for all cells}
  \item{layer}{Layer number (only relevant for RasterBrick objects). If missing, the values of all layers is set}
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

