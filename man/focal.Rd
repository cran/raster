\name{focal}

\alias{focal}

\title{Focal values}

\description{
Calculate "moving window" values for the neighborhood of focal cells using a matrix of weights, perhaps in combination with a function.

This function replaces \code{\link{focal_old}}, \code{\link{focalNA}} and \code{\link{focalFilter}}
}

\usage{
focal(x, w, fun, filename='', na.rm=FALSE, pad=FALSE, padValue=NA, NAonly=FALSE, ...) 
}

\arguments{
\item{x}{RasterLayer}
  
\item{w}{matrix of weights (the moving window), e.g. a 3 by 3 matrix; see Details. The matrix can also be expressed as the number of cells in a single direction or in two directions from the focal cell, in which case the weights are all set to 1. I.e. \code{w=3} refers to a 3 by 3 matrix: 2 cells at each side of the focal cell, queen's case, 9 cells in total. This is equivalent to \code{w=c(3,3)}. You can also specify a rectangular neighborhood, e.g. \code{w=c(3,5)}; but the sides must be odd numbers. If you need even sides, you can add a column or row with weights of zero.}
  
\item{fun}{function (optional). The function fun should take multiple numbers, and return a single number. For example mean, modal, min or max. It should also accept a na.rm argument (or ignore it, e.g. as one of the 'dots' arguments. For example, \code{length} will fail, but \code{function(x, ...){na.omit(length(x))}} works. }

\item{filename}{character. Filename for a new raster (optional)}
 
\item{na.rm}{logical. If \code{TRUE}, \code{NA} will be removed from focal computations. The result will only be \code{NA} if all focal cells are \code{NA}. Except for some special cases (weights of 1, functions like min, max, mean), using \code{na.rm=TRUE} is generally not a good idea in this function because it will unbalance the effect of the weights }
 
\item{pad}{logical. If \code{TRUE}, additional 'virtual' rows and columns are padded to \code{x} such that there are no edge effects. This can be useful when a function needs to have access to the central cell of the filter}
  
\item{padValue}{logical. The value of the cells of the padded rows and columns}
 
\item{NAonly}{logical. If \code{TRUE}, only cell values that are \code{NA} are replaced with the computed focal values}
 
\item{...}{additional arguments. See Details.}  
}

\details{
\code{focal} uses a matrix of weights for the neighborhood of the focal cells. The default function is \code{sum}. It is computationally much more efficient to adjust the weights-matrix than to use another function through the \code{fun} argument. Thus while the following two statements are equivalent (if there are no \code{NA} values), the first one is faster than the second one:

\code{a <- focal2(x, w=matrix(1/9, nc=3, nc=3))}

\code{b <- focal2(x, w=3, fun=mean)}

There is, however, a difference if \code{NA} values are considered. One can use the \code{na.rm=TRUE} option which may make sense when using a function like \code{mean}. However, the results would be wrong when using a weights matrix.


Gaussian filter: \code{filter=matrix(c(1,2,3,2,1,2,3,4,3,2,3,4,5,4,3,2,3,4,3,2,1,2,3,2,1), nrow=5)/65}

Laplacian filter: \code{filter=matrix(c(0,1,0,1,-4,1,0,1,0), nrow=3)}

Sobel filter: \code{filter=matrix(c(1,2,1,0,0,0,-1,-2,-1) / 4, nrow=3)}


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


\author{Robert J. Hijmans}

\examples{
r <- raster(ncols=36, nrows=18, xmn=0)
r[] <- runif(ncell(r)) 

# 3x3 mean filter
r3 <- focal(r, w=matrix(1/9,nrow=3,ncol=3)) 

# 5x5 mean filter
r5 <- focal(r, w=matrix(1/25,nrow=5,ncol=5)) 

# Gaussian filter
gf=matrix(c(1,2,3,2,1,2,3,4,3,2,3,4,5,4,3,2,3,4,3,2,1,2,3,2,1), nrow=5)/65
rg <- focal(r, w=gf)

# The max value for the lower-rigth corner of a 3x3 matrix around a focal cell
f = matrix(c(0,0,0,0,1,1,0,1,1), nrow=3)
f
rm <- focal(r, w=f, fun=max)

# global lon/lat data: no 'edge effect' for the columns
xmin(r) <- -180
r3g <- focal(r, w=matrix(1/9,nrow=3,ncol=3)) 

}


\keyword{spatial}

