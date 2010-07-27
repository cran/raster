\name{factors}

\docType{methods}

\alias{is.factor}
\alias{is.factor,RasterLayer-method}
\alias{is.factor,RasterStack-method}
\alias{asFactor}
\alias{asFactor,ANY-method}
\alias{asFactor,RasterLayer-method}
\alias{asFactor,RasterStack-method}
\alias{levels}
\alias{levels,RasterLayer-method}
\alias{labels}
\alias{labels,RasterLayer-method}
\alias{labels<-}
\alias{as.numeric,RasterLayer-method}
\alias{as.numeric,RasterStack-method}
   
  
\title{Factors}

\description{
Factors are categorical variables. These functions allow for defining a RasterLayer as a categorical variable. 
This feature is under development, and whether a RasterLayer is defined as a factor or not is currently ignored by all other functions.
}

\usage{
is.factor(x)
asFactor(x, ...) 
levels(x)
labels(object, ...)
labels(object) <- value
}

\arguments{
  \item{x}{A RasterLayer or (in some cases) a RasterStack object}
  \item{object}{A RasterLayer object}
  \item{value}{New labels}
  \item{...}{Additional arguments. See under Methods}  
}

\details{
If \code{x} is a RasterLayer, a full call to the asFactor is:
\code{asFactor(x, levels=NULL, labels=NULL) }

\tabular{rll}{
\tab \code{x} \tab a RasterLayer object \cr
\tab \code{levels} \tab Integer. Optional. The factor levels \cr
\tab \code{labels} \tab Character. Optional. The factor labels\cr
}

If \code{x} is a RasterStack, a full call to the asFactor is:
\code{asFactor(x, v, ...) }

\tabular{rll}{
\tab \code{x} \tab a RasterLayer object \cr
\tab \code{v} \tab Integer between 1 and nlayers(x) indicating the layer to make a factor \cr
\tab \code{..} \tab additional arguments for asFactor as above for when \code{x} is a RasterLayer (levels, labels)\cr
}
}

\value{
A RasterLayer/Stack object
}


\author{Robert J. Hijmans}

\examples{
r <- raster(nrow=10, ncol=10)
r[] <- (runif(ncell(r)) * 10)
is.factor(r)
r
r <- asFactor(r)
is.factor(r)
labels(r)
labels(r) <- letters[1:length(labels(r))]
labels(r)
r
r <- as.numeric(r)
is.factor(r)
s <- stack(r, r)
s <- asFactor(s, 2)
is.factor(s)
}

\keyword{methods}
\keyword{spatial}

