\name{rectify}

\alias{rectify}


\title{rectify a Raster object}


\description{
rectify changes a rotated Raster* object into a non-rotated (rectangular) object. This is wrapper function around \code{\link{resample}}.
}


\usage{
rectify(x, ext, res, method='ngb', filename='', ...) 
}

\arguments{
  \item{x}{Raster* object to be rectified}
  \item{ext}{Optional. Extent object or object from which an Extent object can be extracted}
  \item{res}{Optional. Single or two numbers to set the resolution}
  \item{method}{Method used to compute values for the new RasterLayer, should be "bilinear" for bilinear interpolation, or "ngb" for nearest neighbor }
  \item{filename}{Character. Output filename }
  \item{...}{Additional arguments. See Details}    
 }

\details{
The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{progress} \tab Character. "text", "window", or "" (the default, no progress bar)  \cr
 }
}

\value{
A RasterLayer or RasterBrick object, and, in some cases, the side-effect of a file with the values of the new RasterLayer 
}

\author{Robert J. Hijmans }


\keyword{spatial}

