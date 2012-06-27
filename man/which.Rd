\name{which}

\docType{methods}

\alias{Which}
\alias{Which,RasterLayer-method}

\title{Which cells are TRUE?}

\description{
\code{Which} returns a RasterLayer with \code{TRUE} or \code{FALSE}. Cells with 1 are set to \code{TRUE}, all other cells, including \code{NA} are set to \code{FALSE}
}


\usage{
Which(x, ...) 
}

\arguments{
  \item{x}{A RasterLayer object or a logical expression resulting in a RasterLayer object}
  \item{...}{Additional arguments. See Details} 
}

\details{
You can use the additional logical argument \code{cells}. If cells=\code{FALSE} (the default), a (logical) RasterLayer is returned, if cells=\code{TRUE}, the cell numbers are returned for which the condition is \code{TRUE}  
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
s <- Which(r > 0.5, cells=TRUE)
s[1:5]
s <- Which(r > 0.5)
}

\keyword{spatial}
