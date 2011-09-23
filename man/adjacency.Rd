\name{adjacency}

\alias{adjacency}

\title{Pairs of adjacent cells}

\description{
Identify pairs of cells that are adjacent.  
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

\details{
Cell numbers start with 1 in the upper-left corner and increase from left to right and from top to bottom.

Number of directions: 4 connects cells with one-cell rook moves, 8 with one-cell queen moves, and 16 with knight and one-cell queen moves.
"bishop" connects cells with one-cell diagonal moves.

The function connects the outer meridians for global rasters with a longitude/latitude coordinate reference system.
}

\value{
A two column matrix with each row containing a pair of adjacent cells. 
}

\seealso{
\code{\link{adjacent}}
}

\author{Jacob van Etten \email{jacobvanetten@yahoo.com}}


\examples{
r <- raster(nrows=10, ncols=10)
adj <- adjacency(r, fromCells = c(1,30,55,72,100), 
				toCells = 1:ncell(r), directions=4) 
	
# put the result in a RasterLayer
r[] <- 0
r[c(1,30,55,72,100)] <- 1
r[adj[,2]] <- 2
plot(r)


# How many time does one cell value occur next to another cell value?
r <- raster(ncol=10, nrow=10)
set.seed(0)
r[] <- round(runif(ncell(r)) * 5)
a <- adjacency(r, 1:ncell(r), 1:ncell(r), 8)
v1 <- r[a[,1]]
v2 <- r[a[,2]]
m <- matrix(0, nrow=5,ncol=5)
for(i in 1:length(v1)) {m[v1[i], v2[i]] <- m[v1[i], v2[i]] +1}
m

}

\keyword{spatial}
