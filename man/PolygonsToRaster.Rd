\name{polygonsToRaster}

\alias{polygonsToRaster}

\title{Polygons to raster conversion}

\description{
Polygon to raster conversion. A polygon value is transferred to a raster-cell if it covers the center of the cell. 
Either values associated with each polygon, or a polygon ID is transferred. Holes in polygons are recognized if they are correctly specified.
}

\usage{
polygonsToRaster(p, raster, field=0, overlap='last', mask=FALSE, updateRaster=FALSE, updateValue="NA", getCover=FALSE, filename="", silent=FALSE, ...) 
}

\arguments{
  \item{p}{SpatialPolygons or a SpatialPolygonsDataFrame object }
  \item{raster}{Raster* object}
  \item{field}{The index (or colname) of the column in the SpatialPolygonsDataFrame to be transfered to the RasterLayer (see Details)}
  \item{overlap}{Character. Determines what values to assign to cells with overlapping polygons. Choose from \code{'first', 'last', 'sum', 'min'}, or \code{'max'}}
  \item{filename}{Character. Output filename }
  \item{mask}{Logical. If \code{TRUE} the values of the input RasterLayer are 'masked' by the polygons. I.e. cells overlapping with the polygons retain their values, the other cells become NA }
  \item{updateRaster}{Logical. If \code{TRUE}, \code{raster} must be a RasterLayer. Its values are updated where the polygons overlap cells }
  \item{updateValue}{Character. Select cells to be updated (if \code{updateRaster == TRUE}) by their current values. Either \code{'all'}, \code{'NA'}, \code{'!NA'}, or \code{'zero'} } 
  \item{getCover}{Logical. If \code{TRUE}, the fraction of each grid cell that is covered by the polygons is returned (and the values of \code{field, overlap, mask}, and \code{updateRaster} are ignored. The fraction covered is estimated by dividing each cell into 100 subcells and determining presence/absence of the polygon in the center of each subcell}    
  \item{silent}{Logical. If \code{TRUE}, feedback on the polygon count is suppressed}    
  \item{...}{Additional arguments. See Details.}    
}

\details{
For SpatialPolygons, the \code{field} argument is ignored and the polygon index is used (i.e. numbers from 1 to the number of polygons). This is also 
done for SpatialPolygonDataFrame objects when \code{field}==0. If  \code{field} < 0, all polygons get the value 1.

The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{progress} \tab Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""  \cr
 }
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{linesToRaster}}, \code{\link[raster]{pointsToRaster}}}

\examples{ 
p1 <- rbind(c(-180,-20), c(-140,55), c(10, 0), c(-140,-60), c(-180,-20))
hole <- rbind(c(-150,-20), c(-100,-10), c(-110,20), c(-150,-20))
p2 <- rbind(c(-10,0), c(140,60), c(160,0), c(140,-55), c(-10,0))
p3 <- rbind(c(-125,0), c(0,60), c(40,5), c(15,-45), c(-125,0))
pols <- SpatialPolygons( list(  Polygons(list(Polygon(p1), Polygon(hole)), 1), Polygons(list(Polygon(p2)), 2), Polygons(list(Polygon(p3)), 3)))
pols@polygons[[1]]@Polygons[[2]]@hole <- TRUE

r <- raster(ncol=180, nrow=90)
r <- polygonsToRaster(pols, r, overlap='sum')
#plot(r)
#plot(pols, add=T)

## add a polygon
#p5 <- rbind(c(-180,10), c(0,90), c(40,90), c(145,-10),  c(-25, -15), c(-180,0), c(-180,10))
#addpoly <- SpatialPolygons(list(Polygons(list(Polygon(p5)), 1)))
#addpoly <- as(addpoly, "SpatialPolygonsDataFrame")
#addpoly@data[1,1] <- 10
#r2 <- polygonsToRaster(addpoly, r, field=1, updateRaster=TRUE, updateValue="NA")
#plot(r2)
#plot(pols, border="blue", lwd=2, add=TRUE)
#plot(addpoly, add=TRUE, border="red", lwd=2)

## get the percentage cover of polygons in a cell
#r3 <- raster(ncol=36, nrow=18)
#r3 <- polygonsToRaster(pols, r3, getCover=TRUE)

}

\keyword{ spatial }

