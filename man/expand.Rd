\name{expand}

\alias{expand}
\alias{expand,RasterLayer-method}
\alias{expand,RasterLayer,ANY-method}

\title{Expand}

\description{
Expand returns an RasterLayer with a larger spatial extent. 
The output RasterLayer has the extreme minimum and maximum coordinates of the input RasterLayer and input Extent arguments. 
Thus, all of the cells of the original raster are included.
}

\usage{
expand(x, y, ...)
}

\arguments{
  \item{x}{a RasterLayer object}
  \item{y}{Extent object, or any object that is or has an Extent object, or an object from which it can be extracted (such as sp objects)}
  \item{...}{additional arguments. See Details.}  
}


\value{
A new RasterLayer object, and in some cases the side effect of a new file on disk.
}

\details{
The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{filename} \tab filename for the output RasterLayer. Default='' \cr
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{progress} \tab Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""  \cr
 }
}

\author{Robert J. Hijmans}

\seealso{\code{\link[raster]{crop}}, \code{\link[raster]{merge}}}


\examples{
r <- raster(xmn=-150, xmx=-120, ymx=60, ymn=30, ncol=36, nrow=18)
r[] <- 1:ncell(r)
bb <- extent(-180, 0, 0, 90)
re <- expand(r, bb)
}

\keyword{spatial}
