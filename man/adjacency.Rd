\name{adjacency}

\alias{adjacency}

\title{Adjacency}

\description{
 Identify cells that are adjacent to a set of cells on a raster
}

\usage{
 adjacency(raster, fromCells, toCells, directions) 
}

\arguments{
  \item{raster}{an object of the Raster family}
  \item{fromCells}{a vector of cell numbers for which adjacent cells should be calculated}
  \item{toCells}{a vector of cell numbers from which adjacent cells are selected}
  \item{directions}{in how many direction cells should be connected: 4, 8 or 16; or "Bishop"}
}

\details{
Cell numbers start with 1 in the upper-left corner and increase from left to right and from top to bottom.
Number of directions: 4 connects cells with one-cell rook moves, 8 with one-cell queen moves, and 16 with knight and one-cell queen moves.
Bishop connects cells with one-cell diagonal moves (special cases).
The function connects the outer meridians if the raster is not projeced (in a geographic (lat/lon) "projection") and there is 
data at longitudes -180 and 180 degrees.
}

\value{
  a two column matrix with each row containing a pair of adjacent cells. 
}

\author{Jacob van Etten \email{jacobvanetten@yahoo.com}}

\examples{
r <- raster(nrows=10, ncols=10)
adj <- adjacency(raster=r, fromCells = c(1,30,55,72,100), 
				toCells = c(1:ncell(r)), directions=4) 
	
#put the result in a RasterLayer
v <- vector(length=ncell(r))
v[c(1,30,55,72,100)] <- 1
v[adj[,2]] <- 2
r <- setValues(r, v)
#plot(r)

}

\keyword{spatial}
