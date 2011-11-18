\name{edge}

\alias{edge}
\alias{edge,RasterLayer-method}
  
\title{Edge detection}

\description{
Detect edges. Edges are cells that have more than one class in the 4 or 8 cells surrounding it, or, if \code{classes=FALSE}, cells with values and cells with \code{NA}. 
}

\usage{
\S4method{edge}{RasterLayer}(x, filename="", type='inner', classes=FALSE, directions=8, ...)
}

\arguments{
\item{x}{RasterLayer object}
\item{filename}{Character. Filename for the output RasterLayer (optional)}
\item{type}{Character. 'inner', or 'outer' }
\item{classes}{Character. Logical. If \code{TRUE} all different values are (after rounding) distinguished, as well as \code{NA}. If \code{FALSE} (the default) only edges between \code{NA} and non-\code{NA} cells are considered }
\item{directions}{Integer. Which cells are considered adjacent? Should be 8 (Queen's case) or 4 (Rook's case) }
\item{...}{Additional arguments as for \code{\link{writeRaster}}}  
}

\value{
RasterLayer. Cell values are either 1 (and edge) or 0 (not an edge), or \code{NA}
}

\seealso{ \code{\link{focal}}, \code{\link{clump}} }


\author{Robert J. Hijmans}

\examples{
r <- raster(nrow=18, ncol=36, xmn=0)
r[150:250] <- 1
r[251:450] <- 2
plot( edge(r, type='inner') )
plot( edge(r, type='outer') )
plot( edge(r, classes=TRUE) )
}

\keyword{methods}
\keyword{spatial}
