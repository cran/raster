\name{raster-package}

\alias{raster-package}

\docType{package}

\title{ 
Management, analysis and modeling of raster-type geographic data
}

\description{
This packages provides classes and functions to manipulate geographic (spatial) data in 'raster' format. Raster data divides the world in cells (rectangles; pixels) of equal size (in units of the coordinate reference system). Such data are also referred to as 'grid' data.

The package should be particularly useful when using very large datasets that can not be loaded into the computer's memory. Functions will
work correctly, because they will read, process, and write blocks of data, without loading all values into memory. 

Below is a list of functions grouped by theme. See the vignette for more information and some examples: \code{vignette('raster', 'raster')}
}


\details{
 The package implements classes for Raster data (see \link{Raster-class}) and supports
  \itemize{
    \item Creation of Raster* objects from scratch or from file
    \item Handling extremely large raster files
    \item Raster algebra and overlay functions
    \item Distance, neighborhood (focal) and patch functions
    \item Polygon, line and point to raster conversion
    \item Model predictions
    \item Summarizing raster values
    \item Easy access to raster cell-values
    \item Plotting (making maps)
    \item Manipulation of raster extent, resolution and origin
    \item Computation of row, col and cell numbers to coordinates and vice versa
    \item Reading and writing various raster file types
  }
}



\author{
Robert J. Hijmans and Jacob van Etten

Maintainer: Robert J. Hijmans \email{r.hijmans@gmail.com}
}


\section{I. Creating Raster* objects}{

Raster* objects can be created, from scracth or from raster files, with the following functions:
  \tabular{ll}{
    \code{\link{raster}}\tab To create a RasterLayer \cr
    \code{\link{stack}} \tab To create a RasterStack \cr
    \code{\link{brick}} \tab To create a RasterBrick \cr
    \code{\link{addLayer}} \tab Add a layer to a RasterStack or RasterBrick \cr
    \code{\link{dropLayer}} \tab Remove a layer from a RasterStack or RasterBrick \cr
    \code{\link{unstack}} \tab Create a list of RasterLayer objects from a RasterStack \cr
	}
	
RasterLayer, RasterStack, and RasterBrick objects are, as a group, referred to as Raster* objects
}



\section{II. Changing the spatial extent and/or resolution of RasterLayer objects}{

  \tabular{ll}{
    \code{\link{merge}} \tab Combine RasterLayer objects with different extents (but same origin and resolution) \cr
    \code{\link{mosaic}} \tab Combine RasterLayer objects with different extents; using a function to set values for overlapping areas \cr
    \code{\link{crop}} \tab Select a geographic subset of a RasterLayer \cr
    \code{\link{expand}} \tab Enlarge a RasterLayer  \cr
    \code{\link{trim}} \tab Trim a RasterLayer by removing exterior rows and/or columns that only have NA values \cr
    \code{\link{aggregate}} \tab Combine cells to create larger cells \cr
    \code{\link{disaggregate}} \tab Subdivide cells \cr
    \code{\link{shift}} \tab Shift the location of RasterLayer \cr
    \code{\link{flip}} \tab  Flip values horizontally or vertically \cr
    \code{\link{rotate}} \tab Rotate values (for lon/lat data) \cr
    \code{\link{resample}} \tab Warp values to a RasterLayer with a different origin or resolution that can not easily be (dis)aggregated to \cr	
    \code{\link{projectRaster}} \tab Warp values to a RasterLayer with a different coordinate reference system  \cr	
  }
}


\section{III. Raster algebra}{

	\tabular{ll}{
    \code{\link{Arith-methods}} \tab Arith functions (\code{+, -, *, ^, \%\%, \%/\%, /}) \cr
	\code{\link{Math-methods}} \tab Math functions (\code{abs, sqrt, trunc, log, log10, exp, cos, sin, gamma, round} among others)	\cr	
    \code{\link{Logic-methods}} \tab Logic functions (\code{!, &, |}) \cr
    \code{\link{Summary-methods}} \tab Summary functions (\code{mean, Median, max, min, range, prod, sum, any, all}) \cr
    \code{\link{Compare-methods}} \tab Compare functions (\code{==, !=, >, <, <=, >=}) \cr
	}
}  
  
  
\section{IV. Basic computation}{

  \tabular{ll}{
    \code{\link{calc}} \tab Computations on a single RasterLayer, or on a RasterStack or RasterBrick to produce a single RasterLayer \cr
    \code{\link{overlay}} \tab Computations on mutliple RasterLayer objects \cr
    \code{\link{cover}} \tab First layer covers second layer except where the first layer is NA \cr
    \code{\link{mask}} \tab Use values from first layer except where cells of the second layer are NA (these become NA)\cr
    \code{\link{reclass}} \tab Reclassify using a 'from-to-becomes' matrix \cr
    \code{\link{subs}} \tab Substitute values using a 'is-becomes' matrix \cr	
    \code{\link{init}} \tab Initialize cells with new values \cr	
  }
}


\section{V. Spatial contextual computation}{

  \tabular{ll}{
    \code{\link{distance}} \tab Shortest distance to a cell that is not \code{NA}\cr
    \code{\link{gridDistance}} \tab Distance when traversing grid cells that are not \code{NA} \cr
    \code{\link{distanceFromPoints}} \tab Shortest distance to any point in a set of points \cr
    \code{\link{direction}} \tab Direction (azimuth) to or from cells that are not \code{NA}\cr
    \code{\link{focal}} \tab Focal (neighborhood) functions \cr
    \code{\link{focalFilter}} \tab Focal (neighborhood) filters \cr
    \code{\link{edge}} \tab Edge detection \cr
    \code{\link{clump}} \tab Find clumps (patches) \cr
    \code{\link{adjacency}} \tab Identify cells that are adjacent to a set of cells on a raster \cr
    \code{\link{area}} \tab Compute area of cells (for longitude/latitude data) \cr
  }
}


\section{VI. Model predictions}{

  \tabular{ll}{
    \code{\link{predict}} \tab Predict a non-spatial model to a RasterLayer \cr
    \code{\link{interpolate}} \tab Predict a spatial model to a RasterLayer \cr
}
}


\section{VII. Coverting spatial data types}{

  \tabular{ll}{
    \code{\link{pointsToRaster}} \tab Summarizing points by raster cells \cr
    \code{\link{linesToRaster}} \tab Rasterizing lines \cr
    \code{\link{polygonsToRaster}} \tab Rasterizing polygons \cr
    \code{\link{rasterToPoints}} \tab Create points from a RasterLayer \cr
    \code{\link{rasterToPolygons}} \tab Create polygons from a RasterLayer \cr
    \code{\link{rasterToContour}} \tab Contour lines from a RasterLayer \cr
  }
  
You can coerce Raster* objects to Spatial* objects using \code{as}, as in \code{as(object, 'SpatialGridDataFrame')}  
}



\section{VIII. Summarizing}{

  \tabular{ll}{
    \code{\link{cellStats}} \tab Summarize a Raster cell values with a function \cr
    \code{\link{summary}} \tab Summary of the values of a Raster* object (quartiles and mean) \cr
    \code{\link{freq}} \tab Frequency table of Raster cell values \cr
    \code{\link{count}} \tab Count the frequency of a single (range of) value(s) \cr
    \code{\link{crosstab}} \tab Cross-tabulate two RasterLayer objects \cr
    \code{\link{unique}} \tab Get the unique values in a RasterLayer \cr
    \code{\link{zonal}} \tab Summarize a RasterLayer by zones in another RasterLayer \cr
  }
}

\section{IX. Accessing values of Raster* object cells}{

  \tabular{ll}{
    \code{\link{getValues}} \tab Get all cell values (will fail with very large rasters and limited computer memory), or a row of values (safer) \cr
    \code{\link{getValuesBlock}} \tab Get values for a block (a rectangular area defined by start and end row and column \cr
    \code{\link{xyValues}} \tab Use points (optionally with a buffer) to extract cell values \cr
    \code{\link{cellValues}} \tab Use cell numbers to extract cell values \cr
    \code{\link{polygonValues}} \tab Use a SpatialPolygons* object to extract cell values \cr
    \code{\link{focalValues}} \tab Extract values from focal cells\cr
    \code{\link{sampleRandom}} \tab Random sample \cr
    \code{\link{sampleRegular}} \tab Regular sample \cr
    \code{\link{minValue}} \tab The minimum value of the cells of a Raster* object (not always known) \cr
    \code{\link{maxValue}} \tab The maximum value of the cells of a Raster* object (not always known) \cr
    \code{\link{setMinMax}} \tab Compute the minimum and maximum value of a Raster* object if these are not known \cr
  }
You can also use indexing with \code{[} for cell numbers, and \code{[[} for row / column number combinations \cr
}

\section{X. Plotting}{

Maps
  \tabular{ll}{
    \code{\link{plot}} \tab Plot a Raster* object. The main method to create a map \cr
    \code{\link{plotRGB}} \tab Combine three layers (red, green, blue channels) into a single 'real color' image \cr
    \code{\link{plot3D}} \tab 3-dimensional plot of a RasterLayer \cr
    \code{\link{image}} \tab Plot a Raster* using the image function \cr
    \code{\link{persp}} \tab Perspective plot of a RasterLayer \cr
    \code{\link{contour}} \tab Contour plot of a RasterLayer \cr
    \code{\link{filledContour}} \tab Filled contour plot of a RasterLayer \cr
 }

Interacting with a map (plot of a RasterLayer)
  \tabular{ll}{
    \code{\link{zoom}} \tab Zoom in on a plot of a RasterLayer \cr
    \code{\link{click}} \tab Draw points and/or query values of RasterLayer by clicking on a map \cr
    \code{\link{drawPoly}} \tab Create a SpatialPolygons object by drawing it \cr
    \code{\link{drawLine}} \tab Create a SpatialLines object by drawing it \cr
    \code{\link{drawExtent}} \tab Create an Extent object by drawing it \cr	
} 

Other plots 
  \tabular{ll}{
    \code{\link{plot}} \tab x-y plot of values of two RasterLayer objects\cr
    \code{\link{hist}} \tab Histogram of Raster* object values \cr
    \code{\link{density}} \tab Density plot of Raster* object values \cr
    \code{\link{pairs}} \tab Pairs plot for layers in a RasterStack or RasterBrick \cr
  }
}


\section{XI. Getting and setting Raster* dimensions }{

Basic parameters of existing Raster* objects can be obtained, and in most cases changed, with:  
  \tabular{ll}{
    \code{\link{ncol}}\tab The number of columns \cr
    \code{\link{nrow}} \tab The number of rows \cr
    \code{\link{ncell}} \tab The number of cells (can not be set directly, only via ncol or nrow) \cr
    \code{\link{res}} \tab The resolution (x and y) \cr
    \code{\link{xres}} \tab The x resolution (can be set with res) \cr
    \code{\link{yres}} \tab The y resolution (can be set with res)\cr
    \code{\link{xmin}} \tab The minimum x coordinate (or longitude) \cr
    \code{\link{xmax}} \tab The maximum x coordinate (or longitude) \cr
    \code{\link{ymin}} \tab The minimum y coordinate (or latitute) \cr
    \code{\link{ymax}} \tab The maximum y coordinate (or latitute) \cr
	\code{\link{extent}} \tab The extent (minimum and maximum x and y coordinates) \cr
	\code{\link{origin}} \tab The origin of a Raster* object \cr
    \code{\link{projection}} \tab The coordinate reference system (map projection) \cr	
    \code{\link{isLonLat}} \tab Test if an object has a longitude/latitude coordinate reference system \cr
    \code{\link{filename}} \tab Filename to which RasterLayer or RasterBrick is linked \cr	
    \code{\link{band}} \tab Band (layer) of a multi-band file that this RasterLayer is linked to \cr	
    \code{\link{nbands}} \tab How many bands (layers) does the file have? \cr	
    \code{\link{compare}} \tab Compare the basic parameters of two Raster* objects \cr		
    \code{\link{NAvalue}} \tab Get or set the NA value (for reading from file) \cr
  }

If there are values associated with a RasterLayer object (either in memory or via a link to a file) these are lost when you change the number of columns or rows or the resolution. This is not the case when the extent is changed (as the number of columns and rows will not be affected). Simlarly, with \bold{projection} you can set the projection, but this does not transform the data (see projectRaster for that).   
}


\section{XII. Computing row, column, cell numbers and coordinates}{

Cell numbers start at 1 in the upper-left corner. They increase within rows, from left to right, and then row by row from top to bottom. Likewise, row numbers start at 1 at the top of the raster, and column numbrers start at 1 at the left side of the raster. 

  \tabular{ll}{
    \code{\link{xFromCol}} \tab x-coordinates from column numbers \cr
    \code{\link{yFromRow}} \tab y-coordinates from row numbers \cr
    \code{\link{xFromCell}} \tab x-coordinates from row numbers \cr
    \code{\link{yFromCell}} \tab y-coordinates from cell numbers \cr
    \code{\link{xyFromCell}} \tab x and y coordinates from cell numbers \cr
    \code{\link{colFromX}} \tab Column numbers from x-coordinates (or longitute) \cr
    \code{\link{rowFromY}} \tab Row numbers from y-coordinates (or latitute) \cr
    \code{\link{cellFromXY}} \tab Cell numbers from x and y coordinates \cr
    \code{\link{cellFromRowCol}} \tab Cell numbers from row and column numbers \cr
    \code{\link{cellsFromExtent}} \tab Cell numbers from extent object \cr
    \code{\link{validCell}} \tab Is this a valid cell number? \cr
    \code{\link{validCol}} \tab Is this a valid column number? \cr
    \code{\link{validRow}} \tab Is this a valid row number? \cr
  }
}


\section{XIII. Reading data}{
  \tabular{ll}{
    \code{\link{readAll}} \tab Read all values into memory \cr
    \code{\link{values}} \tab Returns the values read with read* \cr
    \code{\link{dataContent}} \tab Are the cell values in memory? \cr
    \code{\link{dataSource}} \tab What is the source of the values (disk or RAM)? \cr	
  }
  Rather then reading data & using values, it is advisable to use \code{\link{getValues}*} functions (see above)
}

  
\section{XIV. Writing data}{

Basic writing
  \tabular{ll}{
    \code{\link{setValues}} \tab Put new values in a Raster* object \cr
    \code{\link{dataContent}} \tab Are the cell values in memory? \cr
    \code{\link{writeRaster}} \tab Write all values of Raster* object to disk \cr
    \code{\link{KML}} \tab Save raster as KML file \cr	
  }

Advanced writing of entire files or parts thereof
  \tabular{ll}{
    \code{\link{blockSize}} \tab Get suggested block size for reading and writing \cr
    \code{\link{writeStart}} \tab Open a file for writing \cr
    \code{\link{writeValues}} \tab Write some values  \cr
    \code{\link{writeStop}} \tab Close the file after writing \cr
	}
	
Managing files in the 'native raster format'
  \tabular{ll}{
    \code{\link{moveRasterFile}} \tab Open a file for writing \cr
    \code{\link{copyRasterFile}} \tab Write some values  \cr
    \code{\link{removeRasterFile}} \tab Close the file after writing \cr
	}
}


\section{XV. Extent objects}{

  \tabular{ll}{
    \code{\link{extent}} \tab Create an extent object \cr
    \code{\link{intersectExtent}} \tab Intersect two extent objects  \cr
    \code{\link{unionExtent}} \tab Combine two extent objects \cr
    \code{\link{alignExtent}} \tab Align an extent with a Raster* object \cr
    \code{\link{roundExtent}} \tab Round the coordinates of an Extent object \cr		
    \code{\link{nudgeExtent}} \tab Floor/ceiling of the coordinates of an Extent object \cr			
    \code{\link{drawExtent}} \tab Create an Extent object by drawing it on top of a map (see plot) \cr		
    \code{\link{polygonFromExtent}} \tab Create a SpatialPolygons object from an Extent object \cr			
  }
It is also possible to use Arithmetic functions with Extent objects (but these work perhaps unexpectedly!)
}


\section{XVI. Options}{

\tabular{ll}{
  \code{\link{setOptions}} \tab Set session options \cr
  \code{\link{showOptions}} \tab  Show session options \cr
  \code{\link{saveOptions}} \tab Save session options to make them persistent \cr
  \code{\link{clearOptions}} \tab Set session options to default values \cr
}
}


\section{XVII. For programmers}{

\tabular{ll}{
  \code{\link{canProcessInMemory}} \tab Test whether a file can be created in memory \cr
  \code{\link{pbCreate}} \tab Initialize a progress bar  \cr
  \code{\link{pbStep}} \tab Take a progress bar step \cr
  \code{\link{pbClose}} \tab Close a progress bar \cr
  \code{\link{openConnection}} \tab Open a file connection \cr
  \code{\link{closeConnection}} \tab Close a file connection \cr
  \code{\link{rasterTmpFile}}  \tab Get a name for a temporary file \cr
}
}


\section{XVIII. Miscellaneous}{

\tabular{ll}{
    \code{\link{getData}} \tab Download and geographic data\cr
    \code{\link{pointDistance}} \tab Distance between points \cr
    \code{\link{readIniFile}} \tab Read a (windows) 'ini' file \cr
    \code{\link{writeRasterHdr}} \tab Write header file for some raster formats \cr
    \code{\link{trim}} \tab Remove leading and trainling blanks from a character string \cr
    \code{\link{ext}} \tab Get or set the extentsion of a filename \cr
    \code{\link{cv}} \tab Coefficient of variation  \cr
    \code{\link{modal}} \tab Modal value  \cr
    \code{\link{Median}} \tab Median value \cr
    \code{\link{sampleInt}} \tab Random sample of integer values \cr
    \code{\link{showTmpFiles}} \tab Show temporary files \cr
    \code{\link{removeTmpFiles}} \tab Remove temporary files \cr
}
}


\section{Acknowledgements}{
We thank the following persons for suggestions to improve the package: 
Roger Bivand, Jonathan Greenberg, Herry Herry, Paul Hiemstra, Rainer Krug, Alice Laborte, John Lewis, Lennon Li, Agustin Lobo, 
Matteo Mattiuzzi, Edzer Pebesma, and Kevin Ummel.
}

\keyword{package}
\keyword{spatial}
