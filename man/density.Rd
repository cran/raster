\name{density}

\alias{density}
\alias{density,RasterLayer-method}
\alias{density,RasterStackBrick-method}

\docType{methods}

\title{Density plot}

\description{
Create a density plots of values in a RasterLayer
}

\usage{
density(x, ...)
}

\arguments{
\item{x}{RasterLayer object}
\item{...}{Additional arguments passed to plot}
}

\section{Methods}{
\describe{
A full call as implemented here:

\code{density(x, layer, maxpixels=100000, plot=TRUE, main, ...)}

\code{layer} can be used to subset the layers to plot in a multilayer object (RasterBrick or RasterStack)

\code{maxpixels} is the maximum number of (randomly sampled) cells to be used for creating the density plot

see \code{\link[graphics]{plot}} for the other arguments
}}


\value{
A density plot (and a density object, returned invisibly of plot=TRUE). 
}

\author{Robert J. Hijmans}


\keyword{spatial}
