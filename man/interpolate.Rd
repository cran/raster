\name{interpolate}

\docType{methods}

\alias{interpolate}
\alias{interpolate,Raster-method}

\title{Interpolate}

\description{
Make a RasterLayer with interpolated values based on a a fitted model object of classes such as 'gstat' or 'Krige'. I.e. these are models that have 'x' and 'y' as independent variables. If x and y are the only independent variables provide an empty (no associated data in memory or on file) RasterLayer for which you want predictions. If there are more spatial predictor variables provide these as a Raster* object in the first argument of the function. If you do not have x and y locations as implicit predictors in your model you should use \code{\link[raster]{predict}} instead.
}

\usage{
interpolate(object, ...) 
}

\arguments{
  \item{object}{a Raster* object}
  \item{...}{Additional arguments. See below, under Methods}  
}

\section{Methods}{
\describe{
 
\code{predict(object, model, filename='', index=1, ...)}
  
\tabular{rll}{  
  \tab  \code{object} \tab a RasterLayer object \cr
  \tab  \code{model}  \tab A fitted model\cr
  \tab  \code{filename}  \tab Output filename for a new raster; if NA the result is not written to a file but returned with the RasterLayer object, in the data slot \cr
  \tab  \code{xyOnly} \tab Logical. If \code{TRUE}, values of the Raster* object are not considered as co-variables; and only x and y (longitude and latitude) are used. This should match the model \cr
  \tab  \code{ext} \tab An Extent object to limit the prediction to a sub-region of \code{x} \cr
  \tab  \code{const}  \tab data.frame. Can be used to add a constant for which there is no Raster object for model predictions. Particulalry useful if the constant is a character-like factor value \cr
  \tab  \code{index}  \tab Integer. To select the column if 'predict.model' returns a matrix with multiple columns \cr
  \tab  \code{na.rm}  \tab Logical. Remove cells with NA values in the predictors before solving the model (and return a NA value for those cells). In most cases this will not affect the output. This option prevents errors with models that cannot handle NA values. \cr  
  \tab  \code{...}  \tab Additional arguments, see below \cr
}

The following additional arguments can be passed, to replace default values
\tabular{rll}{
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{progress} \tab Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""  \cr
 }
}}

\value{
a RasterLayer object
}


\seealso{ \code{\link[raster]{predict}}, \code{\link[gstat]{predict.gstat}}, \code{\link[fields]{Tps}} }

\author{ Robert J. Hijmans}

\examples{
## Thin plate spline interpolation with x and y only
if (require(fields)) {
r <- raster(system.file("external/test.grd", package="raster"))
ra <- aggregate(r, 10)
xy <- data.frame(xyFromCell(ra, 1:ncell(ra)))
v <- getValues(ra)
tps <- Tps(xy, v)
p <- raster(r)
p <- interpolate(p, tps)
p <- mask(p, r)
plot(p)
}

##gstat examples
if (require(gstat)) {
## inverse distance weighted interpolation with gstat
r <- raster(system.file("external/test.grd", package="raster"))
data(meuse)
mg <- gstat(id = "zinc", formula = zinc~1, locations = ~x+y, data=meuse, nmax=7, set=list(idp = .5))
z <- interpolate(r, mg)
z <- mask(z, r)

## kriging
#coordinates(meuse) = ~x+y
#v <- variogram(log(zinc)~1, meuse)
#m <- fit.variogram(v, vgm(1, "Sph", 300, 1))
#g <- gstat(NULL, "log.zinc", log(zinc)~1, meuse, model = m)
#projection(r) <- projection(meuse)
#x <- interpolate(r, g)
}
}

\keyword{methods}
\keyword{spatial}
