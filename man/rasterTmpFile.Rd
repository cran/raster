\name{rasterTmpFile}

\alias{rasterTmpFile}
\alias{removeTmpFiles}
\alias{showTmpFiles}

\title{Temporary files}

\description{
Functions in the raster package create temporary files if the values of an output RasterLayer cannot be stored in memory (RAM). This can happen
when no filename is provided to a function and in functions where you cannot provide a filename (e.g. when using 'raster algebra').

Temporary files are automatically removed at the start of each session. During a session you can use \code{showTmpFiles} to see what is there and \code{removeTmpFiles} to delete all the temporary files.  \code{rasterTmpFile} returns a temporary filename. These can be useful when developing your own functions. }

\usage{
rasterTmpFile(prefix='raster_tmp_')
showTmpFiles()
removeTmpFiles(h=24)
}

\arguments{
  \item{prefix}{Character. Prefix to the filename (which will be followed by 10 random numbers)}
  \item{h}{Numeric. The minimum age of the files in number of hours (younger files are not deleted)}
}

\details{
The default path where the temporary files are stored is returned (can be changed with \code{\link{rasterOptions}}). 
}

\value{
\code{rasterTmpFile} returns a valid file name

\code{showTmpFiles} returns the names (.grd only) of the files in the temp directory

\code{removeTmpFiles} returns nothing
}




\seealso{ \code{\link{rasterOptions}}, \code{\link[base]{tempfile}} }  

\examples{
\dontrun{
rasterTmpFile('mytemp_')
showTmpFiles()
removeTmpFiles(h=24)
}}

\keyword{ spatial }


