\name{zonal}

\alias{zonal}
\alias{zonal,Raster,RasterLayer-method}

\title{Zonal statistics}

\description{
Compute zonal statistics, that is summarized values of a Raster* object for each "zone" defined by a RasterLayer. 

If \code{stat} is a true \code{function}, \code{zonal} will fail (gracefully) for very large Raster objects, but it will work for functions that can be defined as by a character argument. 

The function used should accept a \code{na.rm} argument. For example, if using \code{fun=length} fails, but \code{fun=function(x, ...){length(x)}} works. the \code{...} argument catches the \code{na.rm} argument, even though it is not used by the function in this case. To remove \code{NA} values, you could use this function: \code{fun=function(x, na.rm){ if(na.rm){length(na.omit(x))}else{length(x)}}}
}

\usage{
\S4method{zonal}{Raster,RasterLayer}(x, z, stat='mean', digits=0, na.rm=TRUE, ...) 
}

\arguments{
  \item{x}{Raster* object}
  \item{z}{RasterLayer with codes representing zones}
  \item{stat}{a function to be applied to summarize the values by zone. Either as character: 'mean', 'min', 'max', 'sum'; or as a proper function (see Details) }
  \item{digits}{integer. Number of digits to maintain in 'zones'. By default averaged to an integer (zero digits)}
  \item{na.rm}{logical. If \code{TRUE}, \code{NA} values in \code{x} are ignored}
  \item{...}{additional arguments. One implemented: \code{progress}, as in \code{\link{writeRaster}}}
}

\value{
A matrix with a value for each zone (unique value in \code{zones})
}

\seealso{ See \code{\link{cellStats}} for 'global' statistics (i.e., all of \code{x} is considered a single zone), and \code{\link{extract}} for summarizing values for polygons}

\author{Robert J. Hijmans}

\examples{
r <- raster(ncols=10, nrows=10)
r[] <- runif(ncell(r)) * 1:ncell(r)
z <- r
z[] <- rep(1:5, each=20)
# for big files, use a character value rather than a function
zonal(r, z, 'sum')

# for smaller files you can also provide a function
\dontrun{
zonal(r, z, mean)
zonal(r, z, min)
}

# multiple layers
zonal(stack(r, r*10), z, 'sum')
}

\keyword{spatial}

