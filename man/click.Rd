\name{click}
\alias{click}

\title{ Click on a map}

\description{
  Click on a map (plot) to get values of a Raster* object at a location; and optionally the coordinates and cell number of the location. 
  Multiple locations can be clicked on by specifying \code{n=}. 
}

\usage{
click(object, n=1, id=FALSE, xy=FALSE, cell=FALSE, type = "n", ...)
}


\arguments{
  \item{object}{Raster* object}
  \item{n}{number of clicks on the map}
  \item{id}{Logical. If \code{TRUE}, a numeric ID is shown on the map that corresponds to the row number of the output}
  \item{xy}{Logical. If \code{TRUE}, xy coordinates are included in the output}
  \item{cell}{Logical. If \code{TRUE}, cell numbers are included in the output}
  \item{type}{One of "n", "p", "l" or "o". If "p" or "o" the points are plotted; if "l" or "o" they are joined by lines. See ?locator}
  \item{...}{additional graphics parameters used if type != "n" for plotting the locations. See ?locator}   
 }
 
\value{
The value(s) of \code{object} at the point(s) clicked on; and the coordinates of the center of the cell if \code{xy==TRUE}.
} 

\note{
  The plot only provides the coordinates (xy pair), the values are read from the Raster* object that is passed as an argument.
  Thus you can extract values for a Raster* object that is not plotted, as long as it shares the coordinate system (CRS) 
  with the layer that is plotted.
  
  Unless the process is terminated prematurely values at at most \code{n} positions are determined. 
  The identification process can be terminated by clicking the second mouse button and selecting 'Stop' from the menu,
  or from the 'Stop' menu on the graphics window. 
}


\seealso{ \code{\link[raster]{drawExtent}} }

\author{ Robert J. Hijmans }

\examples{
 r <- raster(system.file("external/test.grd", package="raster"))
#plot(r)
#click(r)
#now click on the plot (map)
}

\keyword{ spatial }
