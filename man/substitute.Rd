\name{substitute}

\docType{methods}

\alias{subs}
\alias{subs,RasterLayer,data.frame-method}

\title{ Substitute values in a RasterLayer }

\description{
Substitute (replace) values in a RasterLayer with values in a \code{data.frame}. The \code{data.frame} should have a column to identify the key (ID) to match with the values of the RasterLayer, and a column with the replacement values. By default these are the first and second column but you can specify other columns with arguments \code{by} and \code{which} (see under Methods).
}

\usage{
subs(x, y, ...) 
}

\arguments{
  \item{x}{A RasterLayer object}
  \item{y}{A data.frame}
  \item{...}{Additional arguments. See under Methods}  
}


\section{Methods}{
\describe{
A full call to \code{subs} is:

\code{subs(x, y, by=1, which=2, subsWithNA=TRUE, filename='', ... ) }

  \item{\code{x}}{RasterLayer}
  \item{\code{y}}{data.frame}
  \item{\code{by}}{The column number or name that is the key (ID) to match a the \code{data.frame} row to a value of the RasterLayer. Default is 1}
  \item{\code{which}}{The column number or name that has the new (replacement) values. Default is 2}
  \item{\code{subsWithNA}}{Logical. If \code{TRUE} values that are not matched become NA. If \code{FALSE}, they retain their original value (which could also be NA. This latter option is handy when you want to replace only one or a few values. }
  \item{\code{filename}}{Character. output filename}
  \item{\code{...}}{Standard additional variables, see below}
  
  

  \item{\code{overwrite}}{Logical. If \code{TRUE}, "filename" will be overwritten if it exists}
  \item{\code{format}}{Character. output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeFormats}}}
  \item{\code{datatype}}{Character. Output data type. See \code{\link[raster]{dataType}}}
  \item{\code{progress}}{Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""}
}  
} 

\details{
You could obtain the same result with \code{\link[raster]{reclass}}, but \code{subs} is more efficient for simple replacement. Use reclass if you want to replace ranges of values with new values. 

You can also replace values using a fitted model. E.g. fit a model to \code{glm} or \code{loess} and then call \link[raster]{predict}
}

\value{
A new RasterLayer object, and in some cases, the side effect of a new file on disk.
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{reclass}}}

\examples{
r <- raster(ncol=10, nrow=10)
r[] <- round(runif(ncell(r)) * 10)
df = data.frame(id=2:8, v=c(10,10,11,11,12:14))
x = subs(r, df)
x2 = subs(r, df, subsWithNA=FALSE)
}

\keyword{methods}
\keyword{spatial}

