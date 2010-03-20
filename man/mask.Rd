\name{mask}

\docType{methods}

\alias{mask}
\alias{mask,RasterLayer,RasterLayer-method}

\title{mask}

\description{
Create a new RasterLayer where all cells that are NA in mask are set to NA, and that has the same values as x in the other cells. 
}

\usage{
mask(x, mask, ...) 
}


\arguments{
  \item{x}{A RasterLayer object}
  \item{mask}{A RasterLayer object}
  \item{...}{Additional arguments. See below, under Methods}  
}

\section{Methods}{
\describe{
A full call to the mask method for a RasterLayer is:

\code{mask(x, mask, filename='', ... ) }
 
 
\tabular{rll}{
\tab \code{x} \tab a RasterLayer object \cr
\tab \code{mask} \tab a RasterLayer object \cr
\tab \code{filename} \tab character. output filename \cr
\tab \code{...} \tab Additional arguments, see Details \cr}
}
}



\details{
If no filename is specified, and the resulting RasterLayer is too large to hold in memory, it is saved to a temporary file.  

The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{progress} \tab Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""  \cr
 }
 
}

\value{
A new RasterLayer object, and in some cases the side effect of a new file on disk.
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

