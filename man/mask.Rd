\name{mask}

\docType{methods}

\alias{mask}
\alias{mask,RasterLayer,RasterLayer-method}
\alias{mask,RasterStackBrick,RasterLayer-method}
\alias{mask,RasterLayer,RasterStackBrick-method}
\alias{mask,RasterStackBrick,RasterStackBrick-method}

\title{Mask values in a Raster object}

\description{
Create a new Raster* object where all cells that are \code{NA} in a 'mask' object are set to \code{NA}, and that has the same values as \code{x} in the other cells. 
}

\usage{
mask(x, mask, ...) 
}


\arguments{
  \item{x}{A Raster* object}
  \item{mask}{A Raster* object}
  \item{...}{Additional arguments. See below, under Methods}  
}

\section{Methods}{
\describe{
The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{filename} \tab character. output filename \cr
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

\author{Robert J. Hijmans}

\examples{
r <- raster(ncol=10, nrow=10)
m <- raster(ncol=10, nrow=10)
r[] <- runif(ncell(r)) * 10
m[] <- runif(ncell(r))
m[m <0.5] <- NA
mr <- mask(r, m)
}

\keyword{methods}
\keyword{spatial}

