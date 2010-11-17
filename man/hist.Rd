\name{hist}

\docType{methods}

\alias{hist}
\alias{hist,RasterLayer-method}
\alias{hist,RasterStackBrick-method}

\title{Histogram}

\description{
Create a histogram of the values of a RasterLayer. NA values are ignored. Large datasets are sampled using \code{maxpixels}.
}

\usage{
hist(x, ...) 
}

\arguments{
  \item{x}{A Raster* object}
  \item{...}{Additional arguments. See under Methods and at \code{\link[graphics]{hist}}}  
}

\section{Methods}{
\describe{
A full call as implemented here:

\code{hist(x, layer, maxpixels=100000, plot=TRUE, main, ...)}

\code{layer} can be used to subset the layers to plot in a multilayer object (RasterBrick or RasterStack)

\code{maxpixels} is the maximum number of (randomly sampled) cells to be used for creating the histogram

see \code{\link[graphics]{hist}} for the other arguments
}}


\value{
This function is principally used for the side-effect of plotting a histogram, but it also returns an S3 object of class 'histogram' (invisibly if \code{plot=TRUE}).
}


\seealso{ \code{\link[raster]{plot}} and \code{\link[graphics]{hist}} }

\examples{ 
r1 <- raster(nrows=50, ncols=50)
r1 <- setValues(r1, runif(ncell(r1)))
r2 <- setValues(r1, runif(ncell(r1)))
rs <- r1 + r2
rp <- r1 * r2
par(mfrow=c(2,2))
plot(rs, main='sum')
plot(rp, main='product')
hist(rs)
a = hist(rp)
a
}

\keyword{methods}
\keyword{spatial}

