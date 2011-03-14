\name{stackSelect}

\alias{stackSelect}

\title{Select cell values from a multi-layer Raster* object}

\description{
Use a single layer object to select cell values from different layers in a multi-layer Raster* object. The single layer \code{y} should have cell values between \code{1} and \code{nlayers(x)}. The values of \code{y} are rounded. 
}

\usage{
stackSelect(x, y, filename='', ...) 
}

\arguments{
  \item{x}{A RasterStack or RasterBrick object}
  \item{y}{A Raster* object with a single layer (e.g. RasterLayer)}
  \item{filename}{Character. Optional output filename}
  \item{...}{Additional arguments. See below, under Methods}  
}

\section{Methods}{
\describe{
The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{progress} \tab Character. "text", "window", or "" (the default, no progress bar)  \cr
}}}

\value{
A new Raster* object, and in some cases the side effect of a new file on disk.
}

\seealso{\code{\link{stackApply}}}

\author{Robert J. Hijmans}

\examples{
r <- raster(ncol=10, nrow=10)
r[] <- 1
s <- stack(r, r+2, r+5)
r[] <- round((runif(ncell(r)))*3)
x <- stackSelect(s, r)
}

\keyword{methods}
\keyword{spatial}

