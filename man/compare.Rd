\name{compare}

\alias{compare}
  
\title{Compare}

\description{
Evaluate whether a two or more rasters have the same extent, number of rows and columns, projection, resolution, and origin (or a subset of these comparisons).
Cell values are not compared by this function. 
}

\usage{
compare(x, ..., extent=TRUE, rowcol=TRUE, prj=TRUE, res=FALSE, orig=FALSE, tolerance=0.05, stopiffalse=TRUE, showwarning=FALSE) 
}

\arguments{
  \item{x}{A Raster* object }
  \item{...}{Additional Raster* objects}
  \item{extent}{Logical. If \code{TRUE}, bounding boxes are compared}
  \item{rowcol}{Logical. If \code{TRUE}, number of rows and columns of the objects are compared}
  \item{prj}{Logical. If \code{TRUE} projections are compared.}
  \item{res}{Logical. If \code{TRUE}, resolutions are compared (redundant when checking extent and rowcol)}
  \item{orig}{Logical. If \code{TRUE}, origins are compared}
  \item{tolerance}{Numeric, difference permissable (relative to the cell resolution) for objects to be 'equal', for non integer numbers in origin and resolution. See ?all.equal }
  \item{stopiffalse}{Logical. If \code{TRUE}, an error will occur if the objects are not the same}
  \item{showwarning}{Logical. If \code{TRUE}, an warning will be given if objects are not the same. Only relevant when \code{stopiffalse} is \code{TRUE}}
 }
  
\author{Robert J. Hijmans}

\examples{
	r1 <- raster()
	r2 <- r1
	r3 <- r1
	compare(r1, r2, r3)
	nrow(r3) <- 10
#	compare(r1, r3)
	compare(r1, r3, stopiffalse=FALSE)
	compare(r1, r3, rowcol=FALSE)
}

\keyword{ spatial }

