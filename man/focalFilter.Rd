\name{focalFilter}

\alias{focalFilter}

\title{Focal filter}

\description{
Calculate values for the neighborhood of focal cells using a matrix of weights
}

\usage{
focalFilter(x, filter, fun=sum, filename="", na.rm=FALSE, pad=TRUE, padValue=NA, ...) 
}

\arguments{
  \item{x}{A RasterLayer object}
  \item{filter}{a matrix of weights. See Details}    
  \item{fun}{a function to apply to the product of the matrix of weights and the values}
  \item{filename}{Output filename for a new raster}
  \item{na.rm}{Boolean. If \code{TRUE}, \code{NA} values are ignored. Except for some special cases, this is generally not a good idea in this function because it will unbalance the effect of the filter!}
  \item{pad}{Boolean. If \code{TRUE}, additional 'virtual' rows and columns are padded to \code{x} such that there are no edge effects. This can be useful when a function needs to have access to the central cell of the filter. }
  \item{padValue}{Boolean. The value of the cells of the padded rows and columns. This value is ignored if \code{x} is unprojected (latiude/longitude) and spans all longitudes (-180 to 180 degrees) }
  \item{...}{additional arguments. See Details.}  
}

\details{
\code{focalFilter} uses a matrix of weights for the neigbhorhood of the focal cells, together with a function (normally \code{sum}). 

For example, \code{filter=matrix(1/9, nrow=3, ncol=3)} would be equivalent to \code{mean} with \code{ngb=3} in the \code{focal} function.

Gaussian filter:
\code{filter=matrix(c(1,2,3,2,1,2,3,4,3,2,3,4,5,4,3,2,3,4,3,2,1,2,3,2,1), nrow=5)/65}

Laplacian filter:
\code{filter=matrix(c(0,1,0,1,-4,1,0,1,0), nrow=3)}

Sobel filter:
\code{filter=matrix(c(1,2,1,0,0,0,-1,-2,-1) / 4, nrow=3)}

Another example:
\code{filter=matrix(c(0,0,0,0,1,1,0,1,1), nrow=3)} and \code{fun=max} returns the max value for the lower-rigth corner of a 3x3 matrix 
around a focal cell

The filter must have uneven sides (e.g., 3x3 or 5x5), but if you need uneven sides, you can add a column or row with weights of zero.


The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{progress} \tab Character. "text", "window", or "" (the default, no progress bar)  \cr
 }

}

\value{
A new RasterLayer object (in the R environment), and in some cases the side effect of a new file on disk.
}

\seealso{ \code{\link[raster]{focal}} }


\author{Robert J. Hijmans}

\examples{
r <- raster(ncols=36, nrows=18)
r[] <- runif(ncell(r)) 
gf=matrix(c(1,2,3,2,1,2,3,4,3,2,3,4,5,4,3,2,3,4,3,2,1,2,3,2,1), nrow=5)/65
rff <- focalFilter(r, filter = gf)
}

\keyword{spatial}

