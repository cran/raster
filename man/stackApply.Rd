\name{stackApply}

\docType{methods}

\alias{stackApply}

\title{Apply a function on subsets of a RasterStack or RasterBrick}

\description{
Apply a function on subsets of a RasterStack or RasterBrick. The layers to be combined are indicated with the vector \code{indices}. 
}

\usage{
stackApply(x, indices, fun, filename='', na.rm=TRUE, ...) 
}

\arguments{
  \item{x}{A Raster* object}
  \item{indices}{A vector of length \code{nlayers(x)} with values between 1 and \code{nlayers(x)} }
  \item{fun}{A function that returns a single value, e.g. \code{mean} or \code{min}, and that takes an 'na.rm' argument}
  \item{na.rm}{Logical. If \code{TRUE}, NA cells are removed from calculations}
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
}
}
}

\value{
A new Raster* object, and in some cases the side effect of a new file on disk.
}

\seealso{\link[raster]{calc}}

\author{Robert J. Hijmans}

\examples{
r <- raster(ncol=10, nrow=10)
r[]=1:ncell(r)
s <- brick(r,r,r,r,r,r)
s <- s * 1:6
b <- stackApply(s, indices=c(1,1,2,2,3,3), fun=sum)
b
}

\keyword{methods}
\keyword{spatial}

