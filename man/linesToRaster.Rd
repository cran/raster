\name{linesToRaster}

\alias{linesToRaster}

\title{Lines to raster}

\description{
Lines to raster conversion. Lines are rasterized by assining values to cells that are crossed by the lines.
}

\usage{
linesToRaster(lns, raster, field=0, overlap='last', mask=FALSE, updateRaster=FALSE, updateValue="NA", filename="", ...)
}

\arguments{
  \item{lns}{\code{SpatialLines} or a \code{SpatialLinesDataFrame} object (sp package), or an object inheriting from \code{SpatialLines}}
  \item{raster}{RasterLayer object}
  \item{field}{The index of the column in \code{lns} (if it is a \code{SpatialLinesDataFrame}) to be transfered to the \code{RasterLayer} (or the name of that field)}
  \item{overlap}{Character. Determines what values to assign to cells with overlapping lines. Choose from \code{'first', 'last', 'sum', 'min'}, or \code{'max'}}
  \item{mask}{Logical. If \code{TRUE} the values of the input RasterLayer are 'masked' by the lines. I.e. cells that are touched by the lines retain their values, the other cells become \code{NA} }
  \item{updateRaster}{Logical. If \code{TRUE} the values of the input RasterLayer are updated where the polygons overlap cells  }
  \item{updateValue}{Character. Select areas to be updated with the lines values (if \code{updateRaster == TRUE}) by the current cell values. Either 'all', 'NA', '!NA', or 'zero' } 
  \item{filename}{Character. output filename }
  \item{...}{Additional arguments. See Details.}  
}

\details{
For SpatialLines, the 'field' argument is ignored. The line index is used as attribute.

The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{progress} \tab Character. "text", "window", or "" (the default, no progress bar)  \cr
 }
}

\author{Robert J. Hijmans }

\seealso{ \code{\link[raster]{polygonsToRaster}}, \code{\link[raster]{pointsToRaster}}}

\examples{

cds1 <- rbind(c(-180,-20), c(-140,55), c(10, 0), c(-140,-60))
cds2 <- rbind(c(-10,0), c(140,60), c(160,0), c(140,-55))
cds3 <- rbind(c(-125,0), c(0,60), c(40,5), c(15,-45))

lines <- SpatialLines(list(Lines(list(Line(cds1)), "1"), Lines(list(Line(cds2)), "2"), Lines(list(Line(cds3)), "3") ))

r <- raster(ncols=90, nrows=45)
r <- linesToRaster(lines, r)
\dontrun{
plot(r)
plot(lines, add=TRUE)

r <- linesToRaster(lines, r, overlap='count')
plot(r)

r[] = 1:ncell(r)
r <- linesToRaster(lines, r, mask=TRUE)
plot(r)
}
}

\keyword{ spatial }
