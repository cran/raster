\name{polygonValues}

\docType{methods}

\alias{polygonValues}
\alias{polygonValues,SpatialPolygons,Raster-method}

\title{ Extract values from a RasterLayer by polygons }

\description{
This method returns the values of the cells of a Raster object that is covered by a polygon. A cell is covered if its center is inside the polygon (but see the \code{weights} option for considering partly covered cells).
}

\usage{
polygonValues(p, r, ...)
}

\arguments{
\item{p}{SpatialPolygons* object}
\item{r}{Raster object}
\item{...}{additional arguments. See details}
}

\details{
There are two related additional arguments:

\code{weights}. If \code{TRUE}, the function returns, for each polygon, a matrix with the cell values and the approximate fraction of each cell covered (rounded to 1/100; and the sum of weights should be ~1. The weights can be used for averaging; see examples. This option can be useful if the polygons are small relative to the cells size of the RasterLayer

\code{cellnumbers}. If \code{cellnumbers=TRUE} and \code{weights=TRUE}, cellnumbers will also be returned. 
}

\note{
Relatively small polygons (e.g. those smaller than a single cell of the RasterLayer), or polygons with an odd shape, may not return any values if they do not cover any RasterLayer cell centers. In some cases, you could use the centroids of such polygons, for example using \code{xyValues(coordinates(p))} or \code{xyValues(coordinates(p), method='bilinear')}
}

\value{
a list as long as the number of polygons in p
}

\seealso{ \code{\link[raster]{cellValues}}, \code{\link[raster]{xyValues}}, \code{\link[raster]{getValues}}, \code{\link[sp]{SpatialPolygons}}  }

\author{Robert J. Hijmans }

\examples{
r <- raster(nrow=18, ncol=9)
r[] <- 1:ncell(r)

cds1 <- rbind(c(-180,-20), c(-160,5), c(-60, 0), c(-160,-60), c(-180,-20))
cds2 <- rbind(c(80,0), c(100,60), c(120,0), c(120,-55), c(80,0))
polys <- SpatialPolygons(list(Polygons(list(Polygon(cds1)), 1), Polygons(list(Polygon(cds2)), 2)))

#plot(r)
#plot(polys, add=TRUE)
v <- polygonValues(polys, r)
v
# mean for each polygon
unlist(lapply(v, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))

# v <- polygonValues(polys, r, weights=TRUE)
# v
## weighted mean for each polygon
# unlist(lapply(v, function(x) if (!is.null(x)) {sum(apply(x, 1, prod)) / sum(x[,2])} else NA  ))
}

\keyword{methods}
\keyword{spatial}
