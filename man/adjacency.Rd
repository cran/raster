\name{adjacency}

\alias{adjacency}

\title{Pairs of adjacent cells}

\description{
Identify pairs of cells that are adjacent. This function is obsolete; use {\link{adjacent}} in stead. 
}

\usage{
adjacency(x, fromCells, toCells, directions) 
}

\arguments{
  \item{x}{Raster* object}
  \item{fromCells}{a vector of cell numbers for which adjacent cells should be calculated}
  \item{toCells}{a vector of cell numbers from which adjacent cells are selected. You can use the \code{\link{adjacent}} function if you want all cells to be considered }
  \item{directions}{in how many direction cells should be connected: 4, 8 or 16; or "bishop"}
}


\value{
A two column matrix with each row containing a pair of adjacent cells. 
}

\seealso{
\code{\link{adjacent}}
}

\author{Jacob van Etten}

\keyword{spatial}
