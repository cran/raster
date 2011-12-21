\name{select}

\docType{methods}

\alias{select}
\alias{select,SpatialPoints-method}
\alias{select,SpatialLines-method}
\alias{select,SpatialPolygons-method}

\title{
Select features from Spatial objects
}

\description{
Select spatial features from SpatialPoints*, SpatialLines*, or SpatialPolygons* by drawing on a plot (map).
}


\usage{
select(x, ...)
}

\arguments{
\item{x}{SpatialPoints*, SpatialLines*, or SpatialPolygons*}
\item{...}{Additional arguments. See Methods}
}


\section{Methods}{
\describe{

A full call is:

\code{select(x, use='rec', draw=TRUE, col='cyan', size=2, ...)}


\tabular{rll}{
\tab \code{use} \tab Character: 'rec' or 'pol'. To use a rectangle or a polygon for selecting \cr
\tab \code{draw} \tab Logical. Add the selected features to the plot\cr
\tab \code{col} \tab Color. Color to use to draw the selected features (when draw=TRUE) \cr
\tab \code{size} \tab size. Size to draw the selected features with (when draw=TRUE) \cr
}
}
}

\seealso{
\code{\link{crop}}
}


\value{
Spatial* object (invisibly)
}

\author{
Robert J. Hijmans
}

\keyword{spatial}

