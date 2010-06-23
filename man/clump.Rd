\name{clump}

\alias{clump}
\alias{clump,RasterLayer-method}

\title{Detect clumps}

\description{
Detect clumps (patches) of connected cells. Each clump gets a unique ID. NA and zero are used as backaground values (i.e. these values are used to seperate clumps)
}

\usage{
clump(x, ...) 
}

\arguments{
  \item{x}{A RasterLayer object}
  \item{...}{additional arguments. See Details.}  
}

\details{
This function is quite slow. It is much quicker for files that can be loaded into memory, particularly when the igraph package is available.

The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{filename}  \tab Output filename. Default = '' \cr
  \tab \code{format}  \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype}  \tab Character. Output data type; can be 'INT', 'FLT', or a complete datatype description, see \code{\link[raster]{dataType}} \cr
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{progress}  \tab Character. "text", "window", or "" (the default, no progress bar)  \cr
 }
}

\value{
A new RasterLayer object (in the R environment), and in some cases the side effect of a new file on disk.
}

\author{Robert J. Hijmans and Jacob van Etten}

\examples{
r <- raster(ncols=10, nrows=10)
r[] <- round(runif(ncell(r))*0.6 )
rc <- clump(r) 
freq(rc)
#plot(rc)
}
\keyword{spatial}

