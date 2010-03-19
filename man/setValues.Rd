\name{setValues}

\alias{setValues}
\alias{setValues,RasterLayer-method}
\alias{setValues,RasterBrick-method}
\alias{setValuesRows}
\alias{clearValues}

\title{Set values}

\description{
Use these functions to assign values to a RasterLayer or RasterBrick object. While you can access the 'values' slot of the objects directly, 
you would do that at your own peril because when setting values, multiple slots need to be changed; which is what these functions do. 
}

\usage{
setValues(object, values, rownr=-1, layer=-1) 
setValuesRows(object, values, startrow, nrows) 
clearValues(object)
}

\arguments{
  \item{object}{A \code{RasterLayer} or \code{RasterBrick} object }
  \item{values}{Data values to associate with the RasterLayer object. There should be values for all cells or for a single row}
  \item{layer}{Layer number (only relevant for RasterStack objects) }
  \item{rownr}{Row number, between 1 and nrow(raster) }
  \item{startrow}{First row for which values are added}
  \item{nrows}{Number of rows}
}


\seealso{ \code{\link[raster]{replacement}} }


\value{
a Raster* object
}
\author{ Robert J. Hijmans }

\examples{ 
rs <- raster(ncol=10, nrow=10)
vals <- 1:ncell(rs)
rs <- setValues(rs, vals)
rs <- clearValues(rs)
rowvals <- 1:ncol(rs)
rs <- setValues(rs, rowvals, 3)
}

\keyword{ spatial }
\keyword{ methods }

