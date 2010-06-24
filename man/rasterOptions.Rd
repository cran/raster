\name{Options}

\alias{setOptions}
\alias{showOptions}
\alias{clearOptions}
\alias{saveOptions}


\title{Global options for the raster package}

\description{
setOptions allows you to set, and inspect, a number of global options used by the raster package.

Most of these options, are used when writing files to disk. They can be ignored by specific functions if the corresponding argument is provided as an argument to these functions.

The default location is returned by \code{rasterTmpDir}. It is the same as that of the R tempdir but you can change it (for the current session) with \code{rasterOptions(tmpdir= }. 

To permanentely set any of these options, you can add them to \code{<your R installation>/etc/Rprofile.site}. For example, to change the default directory used to save temporary files, add a line like this: \code{options(rasterTmpDir='c:/temp/')} to that file. Beware: all raster (*.grd and *.gri) files in that folder will be deleted every time the raster package is loaded.

\code{showOptions} shows the current options

\code{saveOptions} attempts to save the current options (with the exception of the 'todisk' option) to the  \code{/etc/Rprofile.site} file to make them persistent (they will be set when starting a new R session).

\code{clearOptions} resets the options to their default values.
}

\usage{
setOptions(format, overwrite, datatype, tmpdir, progress, timer, chunksize, todisk)
showOptions()
saveOptions()
clearOptions()
}

\arguments{
\item{format}{Character. The default file format to use. See \code{\link[raster]{writeFormats}}}
\item{overwrite}{Logical. The default value for overwriting existing files. If \code{TRUE}, existing files will be overwritten}
\item{datatype}{Character. The default data type to use. See \code{\link[raster]{dataType}}}
\item{tmpdir}{Character. The default location for writing temporary files}
\item{progress}{Character. Valid values are "text",  "window" and "" (the default in most functions, no progress bar)}
\item{timer}{Logical. If \code{TRUE}, the time it took to complete the function is printed}
\item{chunksize}{Integer. Maximum number of cells to read/write in a single chunk while processing (chunk by chunk) disk based Raster* objects}
\item{todisk}{Logical. For debugging only. Default is \code{FALSE} and should normally not be changed. If \code{TRUE}, results are always written to disk, even if no filename is supplied (a temporary filename is used)}
}

\details{
argument \code{format} sets global option \code{rasterFiletype}. The option is ignored by a function if you pass \code{format} as an argument to that function.

argument \code{overwrite} sets global option \code{rasterOverwrite}. The option is ignored by a function if you pass \code{overwrite} as an argument to that function.

argument \code{datatype} sets global option \code{rasterDatatype}. The option is ignored by a function if you pass \code{datatype} as an argument to that function.

argument \code{tmpdir} sets global option \code{rasterTmpDir}.  See \code{\link[raster]{rasterTmpFile}}

argument \code{progress} sets global option \code{rasterProgress}. The option is ignored by a function if you pass \code{progress} as an argument to that function. 
}

\value{
this function is used for the side-effect of setting global options
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[base]{options}}, \code{\link[raster]{rasterTmpFile}} }  


\keyword{ spatial }
