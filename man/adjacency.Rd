\name{adjacency}

\alias{adjacency}

\title{Pairs of adjacent cells}

\description{
Identify pairs of cells that are adjacent.  See \code{\link{adjacent}} for an alternative implementation.
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
