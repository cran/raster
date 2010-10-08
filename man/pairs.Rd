\name{pairs}

\docType{methods}

\alias{pairs}
\alias{pairs,RasterStackBrick-method}

\title{
Pair plots
}

\description{
Pair plots of layers in a RasterStack or RasterBrick
}


\usage{
pairs(x, ...)
}

\arguments{
\item{x}{RasterBrick or RasterStack}
\item{...}{Additional arguments. See Methods}
}



\section{Methods}{
\describe{
	\code{pairs(x, hist=TRUE, cor=TRUE)}

\tabular{rll}{
\tab \code{v} \tab numeric, to select a subset of pairs, e.g. v=1:3 to plot only the first three variables \cr
\tab \code{hist} \tab logical. If \code{TRUE} a histogram of the values is shown on the diagonal\cr
\tab \code{cor} \tab logical. If \code{TRUE} the correlation coefficient is shown in the upper panels \cr
}		
}
}


\author{
Robert J. Hijmans
}

\keyword{spatial}

