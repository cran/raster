\name{saverasterstack}
\alias{stackSave}
\alias{stackOpen}

\title{ Save or open a RasterStack file }

\description{
A RasterStack is a collection of rasters with the same spatial extent and resolution. They can be crated from RasterLayer objects,
or from raster files. These two functions allow you to save the references to raster files and recreate a rasterStack object later.
The values are not saved, only the references to the files.
}

\usage{
stackOpen(stackfile)
stackSave(x, filename)
}

\arguments{
  \item{stackfile}{ Filename for the RasterStack (to save it on disk) }
  \item{x}{ RasterStack object }
  \item{filename}{File name }
}

\details{
  When a RasterStack is saved to a file, only pointers (filenames) to raster datasets are saved, not the data. 
  If the name or location of a raster file changes, the RasterStack becomes invalid.
}

\value{
  a RasterStack object
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{writeRaster}}, \code{\link[raster]{stack}}, \code{\link[raster]{changeStack}}}

\examples{
rasterfile <- system.file("external/test.grd", package="raster")
st <- stack(c(rasterfile, rasterfile))
st <- stackSave(st, "mystack")
# note that filename adds an extension .stk to a stackfile  
#st2 <- stackOpen("mystack.stk")
#st2
}
\keyword{ spatial }

