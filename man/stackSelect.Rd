\name{stackSelect}

\alias{stackSelect}
\alias{stackSelect,RasterStackBrick,Raster-method}


\title{Select cell values from a multi-layer Raster* object}

\description{
Use a Raster* bject to select cell values from different layers in a multi-layer Raster* object. 
The object to select values \code{y} should have cell values between \code{1} and \code{nlayers(x)}. The values of \code{y} are rounded. 
}

\usage{
stackSelect(x, y, ...) 
}

\arguments{
  \item{x}{A RasterStack or RasterBrick object}
  \item{y}{A Raster* object}
  \item{...}{Additional arguments. See below, under Methods}  
}

\section{Methods}{
\describe{
The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{recycle} \tab Logical. Recursively select values (default = \code{FALSE}. Only relevant if \code{y} has multiple layers.
  E.g. if \code{x} has 12 layers, and \code{y} has 4 layers, the indices of the \code{y} layers are used three times \cr
  \tab \code{type} \tab Character. Only relevant when \code{recycle=TRUE}. Can be 'index' or 'truefalse'. If it is 'index', 
  the cell values of \code{y} should represent layer numbers. If it is 'truefalse' layer numbers are indicated by 0 (not used, NA returned)
  and 1 (used)\cr
  \tab \code{filename} \tab Character. Output filename. See \code{\link[raster]{writeRaster}} \cr
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

