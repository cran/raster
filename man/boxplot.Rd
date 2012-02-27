\name{boxplot}

\docType{methods}

\alias{boxplot}
\alias{boxplot,Raster-method}

\title{
Box plot of Raster objects
}

\description{
Box plot of layers in a Raster object
}


\usage{
boxplot(x, ...)
}

\arguments{
\item{x}{Raster* object}
\item{...}{Additional arguments. See Methods}
}


\section{Methods}{
\describe{
	\code{boxplot(x, maxpixels=100000, ...)}

\tabular{rll}{
\tab \code{maxpixels} \tab Integer. Number of pixels to sample from each layer of large Raster objects \cr
\tab \code{...} \tab Argument passed to \code{graphics::\link[graphics]{boxplot}} \cr
}		
}
}

\seealso{
\code{\link{pairs}, \link{hist}}
}

\examples{
r1 <- r2 <- r3 <- raster(ncol=10, nrow=10)
r1[] <- rnorm(ncell(r1), 100, 40)
r2[] <- rnorm(ncell(r1), 80, 10)
r3[] <- rnorm(ncell(r1), 120, 30)
s <- stack(r1, r2, r3)
layerNames(s) <- c('A', 'B', 'C')

boxplot(s, notch=TRUE, col=c('red', 'blue', 'orange'), main='Box plot', ylab='random' )

}

\author{
Robert J. Hijmans
}

\keyword{spatial}

