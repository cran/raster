\name{focal_old}

\alias{focal_old}
\alias{focalNA}

\title{Focal (old functions)}

\description{
These functions are depracated. They have been replaced by the new \link{focal} function (with slightly different arguments).
focal_old was named focal in previous versions and is still included to compare with and transition to the new function.

Calculate values for the neighborhood of focal cells. focalNA keeps original values except where these are \code{NA}. 
New values are computed with function \code{fun}, using the values of the cells in the neigborhood of a focal cell (including the focal cell).
}

\usage{
focal_old(x, ngb=3, fun=mean, na.rm=TRUE, filename="", ...) 
focalNA(x, ngb=3, fun=mean, recursive=FALSE, maxrec=0, filename="", ...) 
}

\arguments{
  \item{x}{A RasterLayer object}
  \item{ngb}{Neighborhood size.  A neighborhood is expressed in number of cells in a single direction or in two direction from the focal cell. I.e. \code{ngb=3} refers to 2 cells at each side of the focal cell, queen's case, 9 cells in total. This is equivalent to \code{ngb=c(3,3)}. You can also specify a rectangular neighborhood, e.g. \code{ngb=c(3,5)}. Neighborhoods are normally odd numbers so that the focal cell is centered, but even numbers are permitted in this function}
  \item{fun}{The function to be applied. The function fun should take multiple numbers, and return a single number. For example mean, modal, min or max. It should also accept a na.rm argument (or ignore it, e.g. as one of the 'dots' arguments. For example, \code{length} will fail, but \code{function(x, ...){na.omit(length(x))}} works}
  \item{na.rm}{Logical. If \code{TRUE}, \code{NA} will be removed from focal computations. The result will only be \code{NA} if all focal cells are \code{NA}}
  \item{recursive}{Logical. If \code{TRUE}, the function will continue recursively untill there are no more cells with \code{NA}}  
  \item{maxrec}{Integer. Maximum number of recursions (if \code{recursive=TRUE})}  
  \item{filename}{Output filename for a new raster}
  \item{...}{additional arguments. See Details.}  
}

\details{
The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
\tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
\tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
\tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
\tab \code{progress} \tab Character. "text", "window", or "" (the default, no progress bar)  \cr
}
}

\value{
RasterLayer
}

\seealso{ \code{\link[raster]{focal}}} 


\author{Robert J. Hijmans}

\examples{
r <- raster(ncols=36, nrows=18)
r[] <- runif(ncell(r)) 
rf <- focal_old(r, fun=mean, ngb=3) 
}

\keyword{spatial}

