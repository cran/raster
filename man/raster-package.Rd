\name{raster-package}

\alias{raster-package}

\docType{package}

\title{ 
Management, analysis and modeling of raster-type geographic data
}

\description{
The raster package provides classes and functions to manipulate geographic (spatial) data in 'raster' format. Raster data divides space into cells (rectangles; pixels) of equal size (in units of the coordinate reference system). Such data are also referred to as 'grid' data.

The package should be particularly useful when using very large datasets that can not be loaded into the computer's memory. Functions will
work correctly, because they they process large files in chunks, i.e., they read, compute, and write blocks of data, without loading all values into memory at once.

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



\section{I. Creating Raster* objects}{

Raster* objects can be created, from scracth, files, or from objects of other classes, with the following functions:
  \tabular{ll}{
    \code{\link{raster}}\tab To create a RasterLayer \cr
    \code{\link{stack}} \tab To create a RasterStack (multiple layers)\cr
    \code{\link{brick}} \tab To create a RasterBrick (multiple layers)\cr
    \code{\link{addLayer}} \tab Add a layer to a Raster* object\cr
    \code{\link{dropLayer}} \tab Remove a layer from a RasterStack or RasterBrick \cr
    \code{\link{unstack}} \tab Create a list of RasterLayer objects from a RasterStack \cr
	}
	
RasterLayer, RasterStack, and RasterBrick objects are, as a group, referred to as Raster* objects
}



\section{II. Changing the spatial extent and/or resolution of Raster* objects}{

  \tabular{ll}{
    \code{\link{merge}} \tab Combine Raster* objects with different extents (but same origin and resolution) \cr
    \code{\link{mosaic}} \tab Combine RasterLayer objects with different extents; using a function to set values for overlapping areas \cr
    \code{\link{crop}} \tab Select a geographic subset of a Raster* object \cr
    \code{\link{expand}} \tab Enlarge a Raster* object \cr
    \code{\link{trim}} \tab Trim a Raster* object by removing exterior rows and/or columns that only have NA values \cr
    \code{\link{aggregate}} \tab Combine cells of a Raster* object to create larger cells \cr
    \code{\link{disaggregate}} \tab Subdivide cells \cr
    \code{\link{shift}} \tab Move the location of Raster \cr
    \code{\link{flip}} \tab  Flip values horizontally or vertically \cr
    \code{\link{rotate}} \tab Rotate values (for lon/lat data) \cr
    \code{\link{resample}} \tab Warp values to a Raster* object with a different origin or resolution \cr	
    \code{\link{projectRaster}} \tab Warp values to a Raster* object with a different coordinate reference system  \cr	
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
  
  
\section{IV. Cell based computation}{

  \tabular{ll}{
    \code{\link{calc}} \tab Computations on a single Raster* object \cr
    \code{\link{stackApply}} \tab Computations on groups of layers in Raster* object \cr
    \code{\link{overlay}} \tab Computations on multiple RasterLayer objects \cr
    \code{\link{cover}} \tab First layer covers second layer except where the first layer is NA \cr
    \code{\link{mask}} \tab Use values from first Raster except where cells of the mask Raster are NA (these become NA)\cr
    \code{\link{cut}} \tab Reclassify values using ranges \cr
    \code{\link{subs}} \tab Reclassify values using a 'is-becomes' matrix \cr	
    \code{\link{reclass}} \tab Reclassify using a 'from-to-becomes' matrix \cr
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


\section{VII. Data type conversion}{

  \tabular{ll}{
    \code{\link{rasterize}} \tab Rasterizing points, lines or polygons\cr
    \code{\link{rasterToPoints}} \tab Create points from a RasterLayer \cr
    \code{\link{rasterToPolygons}} \tab Create polygons from a RasterLayer \cr
    \code{\link{rasterToContour}} \tab Contour lines from a RasterLayer \cr
    \code{\link{rasterFromXYZ}} \tab RasterLayer from regularly spaces points\cr
    \code{\link{raster}} \tab RasterLayer from SpatialGrid*, image, or matrix objects\cr
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
    \code{\link{as.matrix}} \tab Get cell values as a matrix \cr
    \code{\link{as.array}} \tab Get cell values as an array \cr
    \code{\link{extract}} \tab Use cell numbers, points, lines, polygons, or an extent, to extract values from a Raster* object, or extract focal values\cr
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
    \code{\link{text}} \tab Plot the values of a RasterLayer on top of a map \cr
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
    \code{\link{plot}} \tab x-y scatter plot of values of two RasterLayer objects\cr
    \code{\link{hist}} \tab Histogram of Raster* object values \cr
    \code{\link{density}} \tab Density plot of Raster* object values \cr
    \code{\link{pairs}} \tab Pairs plot for layers in a RasterStack or RasterBrick \cr
    \code{\link{boxplot}} \tab Box plot of the values of one or multiple layers\cr
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
	\code{\link{origin}} \tab The origin of a Raster* object\cr
    \code{\link{projection}} \tab The coordinate reference system (map projection) \cr	
    \code{\link{isLonLat}} \tab Test if an object has a longitude/latitude coordinate reference system \cr
    \code{\link{filename}} \tab Filename to which a RasterLayer or RasterBrick is linked \cr	
    \code{\link{band}} \tab Band (layer) of a multi-band file that this RasterLayer is linked to \cr	
    \code{\link{nbands}} \tab How many bands (layers) does the file have? \cr	
    \code{\link{compare}} \tab Compare for equality the basic parameters of two Raster* objects \cr		
    \code{\link{NAvalue}} \tab Get or set the \code{NA} value (for reading from file; see \link{writeRaster} for setting it when writing a file) \cr
  }

If there are values associated with a RasterLayer object (either in memory or via a link to a file) these are lost when you change the number of columns or rows or the resolution. This is not the case when the extent is changed (as the number of columns and rows will not be affected). Simlarly, with \bold{projection} you can set the projection, but this does not transform the data (see \link{projectRaster} for that).   
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

\section{XIII. Writing files}{

Basic writing
  \tabular{ll}{
    \code{\link{setValues}} \tab Put new values in a Raster* object \cr
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
}	


\section{XIV. Extent objects}{

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

\section{XV. Options}{

\tabular{ll}{
  \code{\link{setOptions}} \tab Set session options \cr
  \code{\link{showOptions}} \tab  Show session options \cr
  \code{\link{saveOptions}} \tab Save session options to make them persistent \cr
  \code{\link{clearOptions}} \tab Set session options to default values \cr
}
}


\section{XVI. Miscellaneous}{

\tabular{ll}{
    \code{\link{getData}} \tab Download and geographic data\cr
    \code{\link{pointDistance}} \tab Distance between points \cr
    \code{\link{readIniFile}} \tab Read a (windows) 'ini' file \cr
    \code{\link{hdr}} \tab Write header file for a number of raster formats \cr
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

\section{XVII. For programmers}{

\tabular{ll}{
  \code{\link{canProcessInMemory}} \tab Test whether a file can be created in memory \cr
  \code{\link{pbCreate}} \tab Initialize a progress bar  \cr
  \code{\link{pbStep}} \tab Take a progress bar step \cr
  \code{\link{pbClose}} \tab Close a progress bar \cr
  \code{\link{openConnection}} \tab Open a file connection \cr
  \code{\link{closeConnection}} \tab Close a file connection \cr
  \code{\link{rasterTmpFile}}  \tab Get a name for a temporary file \cr
  \code{\link{inMemory}} \tab Are the cell values in memory? \cr
  \code{\link{fromDisk}} \tab Are the cell values read from a file? \cr	
}
}

\author{
Robert J. Hijmans and Jacob van Etten

Maintainer: Robert J. Hijmans \email{r.hijmans@gmail.com}
}

\section{Acknowledgements}{
We are grateful for the extensive suggestions made by Jonathan Greenberg, Matteo Mattiuzzi, and Steven Mosher. We also thank the following persons for suggestions to improve the package: Roger Bivand, Herry Herry, Paul Hiemstra, Mayeul Kauffmann, Rainer Krug, Alice Laborte, John Lewis, Lennon Li, Agustin Lobo, Edzer Pebesma, David Ramsey, Jon Olav Skoien, and Kevin Ummel.
}

\keyword{package}
\keyword{spatial}
