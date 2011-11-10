\name{expand}

\alias{expand}
\alias{expand,Raster-method}

\title{Expand}

\description{
Expand returns an Raster* object with a larger spatial extent. 
The output Raster object has the outer minimum and maximum coordinates of the input Raster and Extent arguments. 
Thus, all of the cells of the original raster are included. See \code{\link[raster]{crop}} if you (also) want to remove rows or columns. 
}

\usage{
expand(x, y, ...)
}

\arguments{
  \item{x}{RasterLayer object}
  \item{y}{Extent object, or any object that is or has an Extent object, or an object from which it can be extracted (such as sp objects). 
  Alternatively, you can provide a vector of lenght 2 with the number indicating the amount of rows and columns that need to be added (or a single number when the number of rows and columns is equal)}
  \item{...}{Additional arguments as for \code{\link{writeRaster}}}  

}


\value{
RasterLayer or RasterBrick object
}


\author{Robert J. Hijmans}

\seealso{\code{\link[raster]{crop}}, \code{\link[raster]{merge}}}

\examples{
r <- raster(xmn=-150, xmx=-120, ymx=60, ymn=30, ncol=36, nrow=18)
r[] <- 1:ncell(r)
e <- extent(-180, 0, 0, 90)
re <- expand(r, e)

# expand with a number of rows and columns (at each side)
re2 <- expand(r, c(2,10))
}

\keyword{spatial}
