\name{bands}

\alias{band}
\alias{band,RasterLayer-method}
\alias{nbands}
  
\title{Number of bands}

\description{
A 'band' refers to a single layer for a possibly multi-layer file. Most RasterLayer objects will refer to files with a single band. (The term 'band' is frequently used in remote sensing to refer to a variable (layer) in a multi-variable dataset and in that context bands could be stored in a single or in seperate files). 

\code{nbands} returns the number of bands of the file that a RasterLayer points to (and 1 if it does not point at any file). This functions also works for a RasterStack for which it is equivalent to \code{\link{nlayers}}.

\code{band} returns the specific band the RasterLayer refers to (1 if the RasterLayer points at single layer file or does not point at any file). 
}

\usage{
nbands(x)
band(x, ...)
}

\arguments{
  \item{x}{RasterLayer object}
  \item{...}{Additional arguments (none at this time)}
}

\seealso{\code{\link[raster]{nlayers}}}

\value{
a numeric value >= 1
}

\author{Robert J. Hijmans }
\examples{
r <- raster()
nbands(r)
band(r)
}

\keyword{spatial}
