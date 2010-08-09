\name{indexing}
\docType{methods}

\alias{[,Raster-method}
\alias{[,RasterStack,ANY,missing-method}
\alias{[,Raster,ANY,missing-method}

\alias{[[,RasterLayer,ANY,ANY-method}
\alias{[[,RasterStackBrick,ANY,ANY-method}

\alias{[<-,RasterLayer-method}
\alias{[<-,RasterLayer,ANY,missing-method}

\title{Values from index and replacement}

\description{
Show or replace values of 

  \code{object[x]} can be used to access or set values of a Raster* object, using cell numbers as index. 
  
  \code{object[[r,c]]} can be used to access values of a RasterLayer object, using row and column numbers as index. 
}


\section{Methods}{
\describe{

\code{object[x]}

\code{object[x] <- value}

\code{object[[r,c]]}

Arguments
\tabular{rll}{
\tab \code{object} \tab a RasterLayer object \cr
\tab \code{x} \tab cell number(s) \cr
\tab \code{r} \tab row number(s) \cr
\tab \code{c} \tab column number(s) \cr
\tab \code{value} \tab value(s) \cr
}

}}

\seealso{ \code{\link[raster]{values}} , \code{\link[raster]{setValues}} }


\examples{
r <- raster(ncol=10, nrow=5)
r[] <- 1:ncell(r) * 2
getValues(r)
r[1]
r[1:10]
r[3:8] <- NA
r[1:10]

#equivalent to
getValues(r, format='matrix')
r[[1,1]]
r[[1:2,1:2]]
#first column
r[[,1]]
# first row
r[[1,]]
r[[]]

s <- stack(r,sqrt(r))
s[1:3]
}

\keyword{methods}
\keyword{spatial}
