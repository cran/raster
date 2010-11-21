\name{rasterizeObsolete}

\alias{pointsToRaster}
\alias{linesToRaster}
\alias{polygonsToRaster}

\title{Vector to raster conversion}

\description{
Obsolete, see \link{rasterize}
}

\usage{
pointsToRaster(raster, xy, values=1, fun, ...)
linesToRaster(lns, raster, field=0, overlap='last', ...)
polygonsToRaster(p, raster, field=0, overlap='last', ...) 
}

\arguments{
  \item{lns}{\code{SpatialLines} or a \code{SpatialLinesDataFrame} object (sp package), or an object inheriting from \code{SpatialLines}}
  \item{p}{SpatialPolygons or a SpatialPolygonsDataFrame object }
  \item{raster}{Raster* object}
  \item{field}{The index (or colname) of the column in the SpatialPolygonsDataFrame to be transfered to the RasterLayer (see Details)}
  \item{overlap}{Character. Determines what values to assign to cells with overlapping polygons. Choose from \code{'first', 'last', 'sum', 'min'}, or \code{'max'}}
  \item{xy}{Matrix of x and y coordinates (in the same map reference system as the raster) or a SpatialPoints* object}
  \item{values}{Vector of length of the number of points (i.e., dim(xy)[1] if xy is a matrix) with the values to be used; alternatively 
  you can supply a single value which is then assigned to each point. You can also supply a matrix if its number of rows matches the number of points. }
  \item{fun}{Function to be applied }  
  \item{...}{Additional arguments}    
}

\author{Robert J. Hijmans}

\seealso{ \code{\link{rasterize}} }

\keyword{ spatial }

