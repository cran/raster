\name{adjacent}

\alias{adjacent}

\title{Adjacent cells}

\description{
Identify cells that are adjacent to a set of cells on a raster. See \code{\link{adjacency}} for an alternative implementation.
}

\usage{
adjacent(x, cells, directions=4, pairs=TRUE, target) 
}

\arguments{
  \item{x}{Raster* object}
  \item{cells}{vector of cell numbers for which adjacent cells should be found}
  \item{directions}{the number of directions in which cells should be connected: 4 (rook's case), 8 (queen's case), 16, or 'bishop'}
  \item{pairs}{Boolean. If \code{TRUE}, a matrix of pairs of adjacent cells is returned. If \code{FAlSE}, a vector of cells adjacent to \code{from} is returned (perhaps intersected with \code{to})}
  \item{target}{Optional. Vector of target cell numbers that should be considered. All other adjacent cells are ignored}
 }

\details{
Cell numbers start with 1 in the upper-left corner and increase from left to right and from top to bottom.
}


\value{
matrix or vector with adjacent cells. 
}

\seealso{
\code{\link{adjacency}}
}

\author{Robert J. Hijmans}

\examples{
r <- raster(nrows=10, ncols=10)
adjacent(r, cells = c(1,55,100), directions=4, pairs=FALSE) 
}

\keyword{spatial}
