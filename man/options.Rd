\name{Options}

\alias{setOptions}
\alias{showOptions}
\alias{clearOptions}
\alias{saveOptions}


\title{Global options for the raster package}

\description{
setOptions allows you to set, and inspect, a number of global options used by the raster package.

Most of these options are used when writing files to disk. They can be ignored by specific functions if the corresponding argument is provided as an argument to these functions.

The default location is returned by \code{rasterTmpDir}. It is the same as that of the R tempdir but you can change it (for the current session) with \code{rasterOptions(tmpdir= }. 

To permanentely set any of these options, you can add them to \code{<your R installation>/etc/Rprofile.site}. For example, to change the default directory used to save temporary files, add a line like this: \code{options(rasterTmpDir='c:/temp/')} to that file. All raster files in that folder that are older than 24 hrs are deleted when the raster package is loaded.

\code{showOptions} shows the current options.

\code{saveOptions} attempts to save the current options (with the exception of the 'todisk' option) to the  \code{/etc/Rprofile.site} file to make them persistent (they will be set when starting a new R session).

\code{clearOptions} resets the options to their default values.
}

\usage{
setOptions(format, overwrite, datatype, tmpdir, progress, timer, chunksize,
			maxmemory, todisk, setfileext, tolerance, standardnames)
showOptions()
saveOptions()
clearOptions()
}

\arguments{
\item{format}{Character. The default file format to use. See \code{\link[raster]{writeFormats}}}
\item{overwrite}{Logical. The default value for overwriting existing files. If \code{TRUE}, existing files will be overwritten}
\item{datatype}{Character. The default data type to use. See \link[raster]{dataType}}
\item{tmpdir}{Character. The default location for writing temporary files; See \code{\link{rasterTmpFile}}}
\item{progress}{Character. Valid values are "text",  "window" and "" (the default in most functions, no progress bar)}
\item{timer}{Logical. If \code{TRUE}, the time it took to complete the function is printed}
\item{chunksize}{Integer. Maximum number of cells to read/write in a single chunk while processing (chunk by chunk) disk based Raster* objects}
\item{maxmemory}{Integer. Maximum number of cells to read into memory. I.e., if a Raster* object has more than this number of cells, \code{\link[raster]{canProcessInMemory}} will return \code{FALSE} }
\item{todisk}{Logical. For debugging only. Default is \code{FALSE} and should normally not be changed. If \code{TRUE}, results are always written to disk, even if no filename is supplied (a temporary filename is used)}
\item{setfileext}{Logical. Default is \code{TRUE}. If \code{TRUE}, the file extension will be changed when writing (if known for the file type). E.g. GTiff files will be saved with the .tif extension }
\item{tolerance}{Numeric. The tolerance used when comparing the origin and resolution of Raster* objects. Expressed as the fraction of a single cell. This should be a number between 0 and 0.5 }
\item{standardnames}{Logical. Default is \code{TRUE}. Should \code{\link{layerNames}} be standardized to be syntactically valid names (using \code{\link{make.names}}}
}

\value{
None. These functions are used to get or set global options.
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[base]{options}}, \code{\link[raster]{rasterTmpFile}} }  

\keyword{ spatial }
