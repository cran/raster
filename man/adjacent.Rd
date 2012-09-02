\name{adjacent}

\alias{adjacent}

\title{Adjacent cells}

\description{
Identify cells that are adjacent to a set of cells on a raster. See \code{\link{adjacency}} for an alternative implementation.
}

\usage{
adjacent(x, cells, directions=4, pairs=FALSE, target=NULL, sorted=FALSE, 
         include=FALSE, id=FALSE) 
}

\arguments{
  \item{x}{Raster* object}
  \item{cells}{vector of cell numbers for which adjacent cells should be found}
  \item{directions}{the number of directions in which cells should be connected: 4 (rook's case), 8 (queen's case), 16, or 'bishop'. Or a neigborhood matrix (see Details)}
  \item{pairs}{logical. If \code{TRUE}, a matrix of pairs of adjacent cells is returned. If \code{FALSE}, a vector of cells adjacent to \code{cells} is returned}
  \item{target}{optional vector of target cell numbers that should be considered. All other adjacent cells are ignored}
  \item{sorted}{logical. Should the results be sorted? }
  \item{include}{logical. Should the focal cells be included in the result? }
  \item{id}{logical. Should the id of the cells be included in the result? (numbered from 1 to length(cells) }
 }

\details{
A neighborhood matrix indentifies the cells around each cell that are considered adjacent. The matrix should have one, and only one, cell with value 0 (the focal cell); at least one cell with value 1 (the adjacent cell(s)); All other cells are not considered adjacent and ignored.

Cell numbers start with 1 in the upper-left corner and increase from left to right and from top to bottom.
}


\value{
matrix or vector with adjacent cells. 
}

\seealso{
\code{\link{adjacency}}
}

\author{Robert J. Hijmans and Jacob van Etten}

\examples{
r <- raster(nrows=10, ncols=10)
adjacent(r, cells=c(1, 55), directions=8, pairs=TRUE) 

a <- adjacent(r, cell = c(1,55,90), directions=4, sorted=TRUE) 
a

r[c(1,55,90)] <- 1
r[a] <- 2
plot(r)

# same result as above
rook <- matrix(c(NA, 1, NA, 
                  1, 0,  1, 
                 NA, 1, NA), ncol=3, byrow=TRUE)

adjacent(r, cells = c(1,55,90), directions=rook, sorted=TRUE) 

}

\keyword{spatial}
