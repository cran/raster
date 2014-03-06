\name{corrLocal}

\docType{methods}

\alias{corLocal}
\alias{corLocal,RasterLayer,RasterLayer-method}


\title{Local correlation coefficient}

\description{
Local correlation coefficient for two RasterLayer objects
}

\usage{
\S4method{corLocal}{RasterLayer,RasterLayer}(x, y, ngb=5, 
     method=c("pearson", "kendall", "spearman"), test=FALSE, filename='', ...)
}

\arguments{
  \item{x}{RasterLayer}
  \item{y}{RasterLayer}
  \item{ngb}{neighborhood size. Either a single integer or a vector of two integers c(nrow, ncol)}
  \item{method}{character indicating which correlation coefficient is to be used. One of \code{"pearson"}, \code{"kendall"}, or \code{"spearman"}}
  \item{test}{logical. If \code{TRUE}, return a p-value} 
  \item{filename}{character. Output filename (optional)}
  \item{...}{additional arguments as for \code{\link{writeRaster}}}
}

\note{\code{NA} values are omitted}

\value{
numeric
}

\seealso{ \code{\link{cor}}, \code{\link{cor.test}} }


\examples{
b <- stack(system.file("external/rlogo.grd", package="raster"))
b[[2]] <- flip(b[[2]], 'y')

x <- corLocal(b[[1]], b[[2]] )
plot(x)

# only cells where the p-value < 0.1
xm <- mask(x[[1]], x[[2]] < 0.1, maskvalue=FALSE)
plot(xm)


# for global correlation, use the cor function
x <- as.matrix(b)
cor(x, method="spearman")
 
# use sampleRegular for large datasets
x <- sampleRegular(b, 1000)
cor.test(x[,1], x[,2])
}

\keyword{methods}
\keyword{spatial}

