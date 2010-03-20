\name{which}

\docType{methods}

\alias{which}
\alias{which,RasterLayer-method}

\title{Which cells are TRUE?}

\description{
\code{which} returns a RasterLayer with \code{TRUE} or \code{FALSE}. Cells with 1 are set to \code{TRUE}, all other cells, including \code{NA} are set to \code{FALSE}
}


\usage{
which(x, arr.ind=FALSE) 
}

\arguments{
  \item{x}{A RasterLayer object or a logical expression resulting in a RasterLayer object}
  \item{arr.ind}{Logical. If \code{FALSE}, a (logical) RasterLayer is returned, if \code{TRUE}, the cellnumbers are returned for which the condition is \code{TRUE}. Note that this behavior is somewhat different from the generic 'which' in the base package.} 
}

\seealso{ \code{\link[base]{which}} }

\value{
a RasterLayer object
}

\author{Robert J.Hijmans}

\examples{
r <- raster(ncol=10, nrow=10)
r[] <- runif(ncell(r))
r[r< 0.2] <- NA
s <- which(r > 0.5)
}

\keyword{spatial}
