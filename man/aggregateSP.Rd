\name{aggregatesp}

\docType{methods}

\alias{aggregate,SpatialPolygons-method}

\title{Aggregate polygons}

\description{
Aggregate ('dissolve') method for SpatialPolygons to merge polygons if the values of selected variables are the same.
}

\usage{
\S4method{aggregate}{SpatialPolygons}(x, vars=NULL, sums, ...)
}

\arguments{
  \item{x}{SpatialPolygons* object}

  \item{vars}{Character or integer. The variables (column names or numbers) that should be used to aggregate (dissolve) the SpatialPolygons by only maintaining unique combinations of these variables. The default setting is to use no variables and aggregate all polygons. You can also supply a vector with a length of length(row.names(x)) }

  \item{sums}{List with function(s) and variable(s) to summarize. This should be a list of lists in which each element of the main lists has two items. The first item is function (e.g. mean), the second element is a vector of column names (or indices) that need to summarize with that function. Be careful with character and factor variables (you can use, e.g. 'first' \code{function(x)x[1]} or 'last' \code{function(x)x[length(x)]} or \code{modal} for these variables}
  
  \item{...}{Additional arguments (none implemented)}
}


\value{
SpatialPolygons*
}

\seealso{ \code{\link{disaggregate,SpatialPolygons-method}}, \code{\link{merge,SpatialPolygons,SpatialPolygons-method}} }


\author{Robert J. Hijmans}


\keyword{methods}
\keyword{spatial}

