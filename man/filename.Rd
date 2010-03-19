\name{filename}

\alias{filename}
\alias{filename<-}

\title{Filename}

\description{
Get the filename of a Raster* object. You can also set the filename, but normally you should set a filename by providing a 'filename= ' argument to a function that creates a new RasterLayer or RasterBrick* object.
}

\usage{
filename(x)
filename(x) <- value
}

\arguments{
  \item{x}{A Raster* object }
  \item{value}{A filename (only for RasterStack objects }
  
}

\value{
a Raster* object
}


\author{ Robert J. Hijmans }

\examples{ 
r <- raster( system.file("external/test.grd", package="raster") )
filename(r)
}

\keyword{ spatial }

