\name{zonal}

\alias{zonal}

\title{Zonal statistics}

\description{
Compute zonal statistics, values of a Raster* object for each "zone" in a RasterLayer. 
}

\usage{
zonal(x, zones, stat='mean', digits=0, na.rm=TRUE, progress='') 
}

\arguments{
  \item{x}{Raster* object}
  \item{zones}{RasterLayer object with codes representing zones}
  \item{stat}{The function to be applied. Either as character: 'mean', 'min', 'max', 'sum'; or a function (see Details) }
  \item{digits}{Integer. Number of digits to maintain in 'zones'. By default averaged to an integer (zero digits)}
  \item{na.rm}{Logical. If \code{TRUE}, \code{NA} values in \code{x} are ignored}
  \item{progress}{Character. "text", "window", or "" (the default, no progress bar) }   
}

\details{
If \code{stat} is a \code{function}, \code{zonal} will fail (gracefully) for very large RasterLayers.
The function should accept a \code{na.rm} argument. For example, \code{fun=length} will fail, but \code{fun=function(x, ...){length(x)}} will work; the '...' arguments catches the na.rm argument, even though it is not used by the function in this case. To remove \code{NA} values, you could use this function: \code{fun=function(x, na.rm){ if(na.rm){length(na.omit(x))}else{length(x)}}}
}

\value{
A matrix with a value for each zone (unique value in \code{zones})
}

\seealso{ See \code{\link{cellStats}} for 'global' statistics (i.e., all of \code{x} is considered a single zone) }

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

