\name{Extent-class}

\docType{class}

\alias{Extent-class}
\alias{show,Extent-method}

\title{Class "Extent" }

\description{Object of class Extent are used to define the geographic extent (extremes) of objects of the BasicRaster and Raster* classes}

\section{Objects from the Class}{

Objects can be created by with the \code{\link[raster]{extent}} function or via a calls to \code{new("Extent", ...)}.

}
\section{Slots}{
	 \describe{
    \item{\code{xmin}:}{minimum x coordinate}
    \item{\code{xmax}:}{maximum x coordinate}
    \item{\code{ymin}:}{minumum y coordinate}
    \item{\code{ymax}:}{maximum y coordinate}
  }
}

\section{Methods}{
  \describe{
 \item{show}{display values of a Extent object }
 }
}

\author{ Robert J. Hijmans}


\seealso{ \code{\link[raster]{extent}}, \code{\link[raster]{setExtent}} }

\examples{
ext <- extent(-180,180,-90,90)
ext
}

\keyword{classes}
\keyword{spatial}
