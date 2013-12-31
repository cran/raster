\name{boundaries}

\alias{edge}
\alias{edges}
\alias{boundaries}
\alias{boundaries,RasterLayer-method}
  
\title{boundaries (edges) detection}

\description{
Detect boundaries (edges). boundaries are cells that have more than one class in the 4 or 8 cells surrounding it, or, if \code{classes=FALSE}, cells with values and cells with \code{NA}. 
}

\usage{
\S4method{boundaries}{RasterLayer}(x, filename="", type='inner', classes=FALSE, directions=8, ...)
edge(x, ...)
edges(x, ...)
}

\arguments{
\item{x}{RasterLayer object}
\item{filename}{Character. Filename for the output RasterLayer (optional)}
\item{type}{Character. 'inner' or 'outer'}
\item{classes}{Character. Logical. If \code{TRUE} all different values are (after rounding) distinguished, as well as \code{NA}. If \code{FALSE} (the default) only edges between \code{NA} and non-\code{NA} cells are considered}
\item{directions}{Integer. Which cells are considered adjacent? Should be 8 (Queen's case) or 4 (Rook's case)}
\item{...}{Additional arguments as for \code{\link{writeRaster}}} 
}

\value{
RasterLayer. Cell values are either 1 (a border) or 0 (not a border), or \code{NA}
}

\seealso{ \code{\link{focal}}, \code{\link{clump}} }

\examples{
r <- raster(nrow=18, ncol=36, xmn=0)
r[150:250] <- 1
r[251:450] <- 2
plot( boundaries(r, type='inner') )
plot( boundaries(r, type='outer') )
plot( boundaries(r, classes=TRUE) )
}

\keyword{methods}
\keyword{spatial}
