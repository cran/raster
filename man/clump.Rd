\name{clump}

\alias{clump}
\alias{clump,RasterLayer-method}

\title{Detect clumps}

\description{
Detect clumps (patches) of connected cells. Each clump gets a unique ID. NA and zero are used as background values (i.e. these values are used to seperate clumps). You can use queen's or king's case, using the \code{directions} argument. For larger files that are processed in chunks, the highest clump number is not necessarily equal to the number of clumps (unless you use argument \code{gaps=FALSE}).
}

\usage{
clump(x, ...) 
}

\arguments{
  \item{x}{A RasterLayer object}
  \item{...}{additional arguments. See Details.}  
}

\details{
This function requires that the igraph package is available.


The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{directions}  \tab Integer. Which cells are considered adjacent? Default value is 8 (Queen's case), the only valid alternative is 4 (King's case) \cr
  \tab \code{gaps}  \tab Logical. If \code{TRUE} (the default), there may be 'gaps' in the chunk numbers (e.g. you may have clumps with IDs 1, 2, 3 and 5, but not 4). If it is \code{FALSE}, these numbers will be recoded from 1 to n (4 in this example) \cr
  \tab \code{filename}  \tab Output filename. Default = '' \cr
  \tab \code{format}  \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype}  \tab Character. Output data type; can be 'INT', 'FLT', or a complete datatype description, see \code{\link[raster]{dataType}} \cr
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{progress}  \tab Character. "text", "window", or "" (the default, no progress bar)  \cr
 }
}

\value{
A new RasterLayer object (in the R environment), and in some cases the side effect of a new file on disk.
}

\author{Robert J. Hijmans and Jacob van Etten}

\examples{
r <- raster(ncols=10, nrows=10)
r[] <- round(runif(ncell(r))*0.6 )
rc <- clump(r) 
freq(rc)
#plot(rc)
}
\keyword{spatial}

