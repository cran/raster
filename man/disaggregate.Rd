\name{disaggregate}

\alias{disaggregate}
\alias{disaggregate,RasterLayer,numeric-method}

\title{Disaggregate}

\description{
Disaggregate a RasterLayer to create a new RasterLayer with a higher resolution (smaller cells). The values in the new RasterLayer are the same as in the larger original cells unless you specify \code{method="bilinear"}, in which case values are locally interpolated (using the \code{\link[raster]{resample}} function).
}

\usage{
disaggregate(x, fact, ...) 
}

\arguments{
  \item{x}{a RasterLayer object}
  \item{fact}{degree of aggregation or disaggregation expressed as number of cells (horizontally and vertically). See details}
  \item{...}{additional arguments. See Details.}  
}

\details{
Fact can be a single integer or two integers c(x,y), in which case the first one is the horizontal disaggregation factor and y the vertical disaggreation factor. 
If a single integer values is supplied, cells are disaggregated with the same factor in x and y direction.

If no filename is specified, and the resulting RasterLayer is too large to hold in memory, it is saved to a temporary file.  

The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{method}  \tab Characer. Should be missing or 'bilinear' for bilinear interpolation  \cr
  \tab \code{filename}  \tab Filename for the output RasterLayer \cr
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{overwrite} \tab if \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{progress} \tab Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""  \cr
 }
}

\value{
A new RasterLayer object, and in some cases the side effect of a new file on disk.
}

\seealso{ \code{\link[raster]{aggregate}} }

\author{Robert J. Hijmans}

\examples{
r <- raster(ncols=10, nrows=10)
rd <- disaggregate(r, fact=c(10, 2))
ncol(rd)
nrow(rd)
r[] <- 1:ncell(r)
rd <- disaggregate(r, fact=c(4, 2), method='bilinear')

}

\keyword{spatial}

