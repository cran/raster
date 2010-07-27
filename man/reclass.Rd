\name{reclass}

\docType{methods}

\alias{reclass}
\alias{reclass,RasterLayer-method}

\title{Reclassify}

\description{
Reclassify values of a RasterLayer. The function (re)classifies groups of values to other values. E.g. All values between 1 and 10 become 1, and all values between 11 and 15 become 2. 
Reclassification is done with matrix "rcl".  

Reclassification is applied to  \code{from <= x <= to}. Unless \code{update=TRUE} Reclassification is done in the order of the reclass table. Thus there are overlapping ranges, the last range applies.  
}

\usage{
reclass(x, rcl, ...) 
}

\arguments{
  \item{x}{A RasterLayer object}
  \item{rcl}{Matrix (or vector) for reclassifcation. This matrix must have 3 columns. The first two columns are "from" "to" of the input values, and the third column has the new value for that range.
(You can also supply a vector that can be coecred into a n*3 matrix (by row))}
  \item{...}{additional arguments. See Details.}  
}

\details{

The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{update} \tab Logical. If \code{update=TRUE}, reclass can update values that were classified according to an earlier row in the reclass table. For example if row 1 has \code{1, 10, 15} and row 2 has \code{11, 20, 25}, all the values from 1 to 20 will be classified as 25. \cr
  \tab \code{filename} \tab Output filename (can be absent for RasterLayers that can be stored in memory) \cr
  \tab \code{format}  \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype}  \tab Character. Output data type; can be 'INT', 'FLT', or a complete datatype description, see \code{\link[raster]{dataType}} \cr
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{progress}  \tab Character. "text", "window", or "" (the default, no progress bar)  \cr
 }
}

\value{
A new RasterLayer object, and in some cases the side effect of a new file on disk.
}

\seealso{ \code{ \link[raster]{calc}} } 


\author{Robert J. Hijmans}

\examples{

r <- raster(ncols=36, nrows=18)
r[] <- runif(ncell(r)) 
# reclassify the values into three groups 
# all values >= 0 and <= 0.25 become 1, etc.
m <- c(0, 0.25, 1,  0.25, 0.5, 2,  0.5, 1, 3)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- reclass(r, rclmat)

# equivalent to
rc <- reclass(r, c(-Inf,0.25,1, 0.25,0.5,2, 0.5,Inf,3))
}

\keyword{spatial}
