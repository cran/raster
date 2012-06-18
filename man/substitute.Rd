\name{substitute}

\docType{methods}

\alias{subs}
\alias{subs,Raster,data.frame-method}

\title{ Substitute values in a Raster* object}

\description{
Substitute (replace) values in a Raster* object with values in a \code{data.frame}. The \code{data.frame} should have a column to identify the key (ID) to match with the cell values of the Raster* object, and one or more columns with replacement values. By default these are the first and second column but you can specify other columns with arguments \code{by} and \code{which}.
}

\usage{
\S4method{subs}{Raster,data.frame}(x, y, by=1, which=2, subsWithNA=TRUE, filename='', ...)
}


\arguments{
  \item{x}{Raster* object}
  \item{y}{data.frame}
  \item{by}{The column number or name that is the key (ID) to match a the \code{data.frame} row to a value of the Raster object. Default is 1}
  \item{which}{The column number or name that has the new (replacement) values. Default is 2}
  \item{subsWithNA}{Logical. If \code{TRUE} values that are not matched become NA. If \code{FALSE}, they retain their original value (which could also be NA. This latter option is handy when you want to replace only one or a few values}
  \item{filename}{Character. output filename}
  \item{...}{Additional arguments as for \code{\link{writeRaster}}}  
}


\details{
You could obtain the same result with \code{\link[raster]{reclass}}, but \code{subs} is more efficient for simple replacement. Use reclass if you want to replace ranges of values with new values. 

You can also replace values using a fitted model. E.g. fit a model to \code{glm} or \code{loess} and then call \link[raster]{predict}
}

\value{
A new Raster object, and in some cases, the side effect of a new file on disk.
}

\author{Robert J. Hijmans}

\seealso{ \code{\link{reclass}, \link{cut}}}

\examples{
r <- raster(ncol=10, nrow=10)
r[] <- round(runif(ncell(r)) * 10)
df <- data.frame(id=2:8, v=c(10,10,11,11,12:14))
x <- subs(r, df)
x2 <- subs(r, df, subsWithNA=FALSE)

df$v2 <- df$v * 10
x3 <- subs(r, df, which=2:3)

s <- stack(r, r*3)
names(s) <- c('first', 'second')
x4 <- subs(s, df)
x5 <- subs(s, df, which=2:3)
}

\keyword{methods}
\keyword{spatial}

