\name{Function writing}

\alias{closeConnection}
\alias{openConnection}
\alias{canProcessInMemory}
\alias{pbCreate} 
\alias{pbStep} 
\alias{pbClose} 

\title{ Helper functions to use in new functions you develop}

\description{ 
low level functions for programmers 

\code{canProcessInMemory} is typically used within functions. In the raster package this function is used to determine if the amount of memory needed for the function is available. If there is not enough memory available, the function returns \code{FALSE}, and the function that called it will write the results to a temporary file. 

openConnection opens a file connection for reading, closeConnection removes it. 

pbCreate creates a progress bar, pbStep sets the progress, and pbClose closes it.
}

\usage{
canProcessInMemory(raster, n=4)
closeConnection(raster)
openConnection(raster, silent=FALSE)
pbCreate(nsteps, type, style=3)
pbStep(pb, step=NULL, label='step')
pbClose(pb, time=FALSE)
}

\arguments{
  \item{raster}{a RasterLayer object}
  \item{n}{the number of copies of a RasterLayer object with values that a function needs to store in memory}  
  \item{silent}{Logical. passed on to GDAL.open}  
  \item{nsteps}{Integer. Number of steps the progress bar will make from start to end (e.g. nrow(raster)) }
  \item{type}{Character. 'text', 'tcltk', or 'windows'}
  \item{style}{style for text progress bar. See \code{\link[utils]{txtProgressBar}} }
  \item{pb}{a progress bar object created with pbCreate }
  \item{step}{which step is this ( 1 <= step <= nsteps ). If step is \code{NULL}, a single step is taken }
  \item{label}{label for the non-text progress bars}
  \item{time}{Logical. If \code{TRUE}, time to completion will be printed}
}

\value{
canProcessInMemory: logical

closeHandle: RasterLayer object
}

\author{Robert J. Hijmans}

\examples{
r <- raster(nrow=100, ncol=100)
canProcessInMemory(r, 4)
r <- raster(nrow=100000, ncol=100000)
canProcessInMemory(r, 2)
}

\keyword{ spatial }
