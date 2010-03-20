\name{writeFormats}

\alias{writeFormats}

\title{File types for writing}

\description{
List supported file types for writing RasterLayer values to disk
}

\usage{
writeFormats()
}

\details{
writeFormats returns a matrix of the file formats (the "drivers") that are supported.

Supported formats include:

\tabular{rll}{
\tab \bold{Format} \tab \bold{Long name} \cr
\tab \code{raster} \tab 'Native' raster package format \cr
\tab \code{ascii}  \tab ESRI Ascii \cr
\tab \code{ENVI}   \tab ENVI .hdr Labelled \cr
\tab \code{EHdr}   \tab ESRI .hdr Labelled \cr
\tab \code{ERS}  \tab ERMapper .ers Labelled \cr
\tab \code{GTiff}  \tab GeoTiff \cr
\tab \code{HFA}  \tab Erdas Imagine (.img) \cr
\tab \code{ILWIS}   \tab ILWIS raster \cr
\tab \code{PAux}   \tab PCI .aux Labelled \cr
\tab \code{PCIDSK}   \tab PCIDSK Database File \cr
\tab \code{RST}  \tab  Idrisi Raster A.1 \cr
}
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[rgdal]{GDALDriver-class}} }  

\examples{ 
writeFormats() 
}

\keyword{ spatial }
