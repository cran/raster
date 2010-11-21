\name{factors}

\docType{methods}

\alias{is.factor}
\alias{is.factor,Raster-method}
\alias{is.factor,RasterStack-method}
\alias{asFactor}
\alias{asFactor,ANY-method}
\alias{asFactor,RasterLayer-method}
\alias{asFactor,RasterBrick-method}
\alias{labels}
\alias{labels,Raster-method}
\alias{labels,RasterStack-method}
\alias{labels<-}
\alias{labels<-,RasterLayer,list-method}
\alias{labels<-,RasterBrick,list-method}
   
  
\title{Factors}

\description{
Factors are categorical variables. These functions allow for defining a layer as a categorical variable. 
This feature is under development, and whether a layer is defined as a factor or not is currently ignored by all other functions.
}

\usage{
is.factor(x)
asFactor(x, ...)
labels(object, ...)
labels(object) <- value
}

\arguments{
  \item{x}{A Raster* object}
  \item{object}{A Raster* object}
  \item{value}{list}
  \item{...}{Additional arguments}
}


\value{
Raster* object
}


\author{Robert J. Hijmans}

\examples{
r <- raster(nrow=10, ncol=10)
r[] <- (runif(ncell(r)) * 10)
is.factor(r)
}

\keyword{methods}
\keyword{spatial}

