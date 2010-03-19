\name{trim}

\alias{trim}
\alias{trim,RasterLayer-method}
\alias{trim,character-method}

\title{Trim}

\description{
Trim (shrink) a RasterLayer by removing outer rows and columns that have no data.

Or remove the whitespace before or after a string of characters.
}

\usage{
trim(x, ...) 
}

\arguments{
  \item{x}{A RasterLayer object or a character string}
  \item{...}{additional arguments. See Details.}  
}

\details{
If \code{x} is a RasterLayer, the following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{padding}  \tab Integer. Number of outer rows/columns with NA values to keep; default=0\cr
  \tab \code{filename}  \tab Output filename. Default = '' \cr
  \tab \code{format}  \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype}  \tab Character. Output data type; can be 'INT', 'FLT', or a complete datatype description, see \code{\link[raster]{dataType}} \cr
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{progress}  \tab Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""  \cr
 }
}

\value{
If \code{x} is a RasterLayer, a new RasterLayer object (in the R environment), and in some cases the side effect of a new file on disk.

A character string If \code{x} is a character string.
}

\author{Robert J. Hijmans and Jacob van Etten}

\examples{
r1 <- raster(ncol=36,nrow=18)
r1[309:310] <- 1:2
r1[353:355] <- 3:5
r1[400] <- 6
s <- trim(r1) 


trim("  hi folks  ")
}

\keyword{spatial}

