\name{focal_old}

\alias{focal_old}
\alias{focalFilter}

\title{Focal (old functions)}

\description{
These functions are depracated. They have been replaced by the new \code{\link{focal}} function (with slightly different arguments).
focal_old was called focal in previous versions and is still included to compare with and transition to the new function.
}

\usage{
focal_old(x, ngb=3, fun=mean, na.rm=TRUE, filename="", ...) 
focalFilter(x, filter, fun=sum, filename="", na.rm=FALSE, pad=TRUE, padValue=NA, ...) 
}

\arguments{
  \item{x}{A RasterLayer object}
  \item{ngb}{Neighborhood size.  A neighborhood is expressed in number of cells in a single direction or in two direction from the focal cell}
  \item{fun}{The function to be applied. The function fun should take multiple numbers, and return a single number and accept argument \code{na.rm}}
  \item{na.rm}{Logical. If \code{TRUE}, \code{NA} will be removed from focal computations}
  \item{filename}{Output filename for a new raster}
  \item{filter}{a matrix of weights. See Details}    
  \item{pad}{Boolean. If \code{TRUE}, additional 'virtual' rows and columns are padded to \code{x} such that there are no edge effects. }
  \item{padValue}{Boolean. The value of the cells of the padded rows and columns. }
  \item{...}{additional arguments}  
}  



\value{
RasterLayer
}

\seealso{ \code{\link[raster]{focal}}} 

\author{Robert J. Hijmans}

\keyword{spatial}

