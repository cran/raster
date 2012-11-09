 \name{predict}

\docType{methods}

\alias{predict}
\alias{predict,Raster-method}

\title{Spatial model predictions}

\description{
Make a Raster object with predictions from a fitted model object (for example, obtained with \code{lm}, \code{glm}). The first argument is a Raster object with the independent (predictor) variables. The \code{\link{names}} in the Raster object should exactly match those expected by the model. This will be the case if the same Raster object was used (via \code{extract}) to obtain the values to fit the model (see the example). Any type of model (e.g. glm, gam, randomForest) for which a predict method has been implemented (or can be implemented) can be used. 
}

\usage{
\S4method{predict}{Raster}(object, model, filename="", fun=predict, ext=NULL, const=NULL, 
   index=1, na.rm=TRUE, inf.rm=FALSE, format, datatype, overwrite=FALSE, progress='', ...)
}


\arguments{
  \item{object}{Raster* object. Typicially a multi-layer type (RasterStack or RasterBrick)}
  \item{model}{A fitted model of any class that has a 'predict' method (or for which you can supply a similar method as \code{fun} argument. E.g. glm, gam, or randomForest }
  \item{filename}{Optional output filename }
  \item{fun}{Function. Default value is 'predict', but can be replaced with e.g. predict.se (depending on the type of model), or your own custom function.}
  \item{ext}{An Extent object to limit the prediction to a sub-region of \code{x} }
  \item{const}{data.frame. Can be used to add a constant for which there is no Raster object for model predictions. Particularly useful if the constant is a character-like factor value for which it is currently not possible to make a RasterLayer }
  \item{index}{Integer. To select the column if predict.'model' returns a matrix with multiple columns }
  \item{na.rm}{Logical. Remove cells with \code{NA} values in the predictors before solving the model (and return a \code{NA} value for those cells). This option prevents errors with models that cannot handle \code{NA} values. In most other cases this will not affect the output. An exception is when predicting with a boosted regression trees model because these return predicted values even if some (or all!) variables are \code{NA} }
  \item{inf.rm}{Logical. Remove cells with values that are not finite (some models will fail with -Inf/Inf values). This option is ignored when \code{na.rm=FALSE}}
 \item{format}{Character. Output file type. See \link[raster]{writeRaster} (optional) }
 \item{datatype}{Character. Output data type. See \link[raster]{dataType} (optional) }
 \item{overwrite}{Logical. If TRUE, "filename" will be overwritten if it exists }
 \item{progress}{Character. "text", "window", or "" (the default, no progress bar)  }
  \item{...}{Additional arguments to pass to the predict.'model' function }
 }

\seealso{
Use \code{\link[raster]{interpolate}} if your model has 'x' and 'y' as implicit independent variables (e.g., in kriging).
}

\value{
RasterLayer or RasterBrick
}



\examples{
# A simple model to predict the location of the R in the R-logo using 20 presence points 
# and 50 (random) pseudo-absence points. This type of model is often used to predict
# species distributions. See the dismo package for more of that.

# create a RasterStack or RasterBrick with with a set of predictor layers
logo <- brick(system.file("external/rlogo.grd", package="raster"))
names(logo)

\dontrun{
# the predictor variables
par(mfrow=c(2,2))
plotRGB(logo, main='logo')
plot(logo, 1, col=rgb(cbind(0:255,0,0), maxColorValue=255))
plot(logo, 2, col=rgb(cbind(0,0:255,0), maxColorValue=255))
plot(logo, 3, col=rgb(cbind(0,0,0:255), maxColorValue=255))
par(mfrow=c(1,1))
}

#get presence and pseudo-absence points
p <- matrix(c(48, 48, 48, 53, 50, 46, 54, 70, 84, 85, 74, 84, 95, 85, 
   66, 42, 26, 4, 19, 17, 7, 14, 26, 29, 39, 45, 51, 56, 46, 38, 31, 
   22, 34, 60, 70, 73, 63, 46, 43, 28), ncol=2)
# 
a <- cbind(runif(250)*(xmax(logo)-xmin(logo))+xmin(logo), 
           runif(250)*(ymax(logo)-ymin(logo))+ymin(logo))

#extract values for points
xy <- rbind(cbind(1, p), cbind(0, a))
v <- data.frame(cbind(xy[,1], extract(logo, xy[,2:3])))
colnames(v)[1] <- 'pa'

#build a model, here an example with glm 
model <- glm(formula=pa~., data=v)

#predict to a raster
r1 <- predict(logo, model, progress='text')

plot(r1)
points(p, bg='blue', pch=21)
points(a, bg='red', pch=21)

# use a modified function to get a RasterBrick with p and se
# from the glm model. The values returned by 'predict' are in a list,
# and this list needs to be transformed to a matrix

predfun <- function(model, data) {
  v <- predict(model, data, se.fit=TRUE)
  cbind(p=as.vector(v$fit), se=as.vector(v$se.fit))
}

# predfun returns two variables, so use index=1:2
r2 <- predict(logo, model, fun=predfun, index=1:2)

# principal components of a RasterBrick
# here using sampling to simulate an object too large
# too feed all its values to prcomp
sr <- sampleRandom(logo, 100)
pca <- prcomp(sr)

# note the use of the 'index' argument
x <- predict(logo, pca, index=1:3)
plot(x)

\dontrun{
require(randomForest)
rfmod <- randomForest(pa ~., data=v)

## note the additional argument "type='response'" that is 
## passed to predict.randomForest
r3 <- predict(logo, rfmod, type='response', progress='window')

## get a RasterBrick with class membership probabilities
v$pa <- as.factor(v$pa)
rfmod2 <- randomForest(pa ~., data=v)
r4 <- predict(logo, rfmod2, type='prob', index=1:2)
spplot(r4)

}
}

\keyword{methods}
\keyword{spatial}
