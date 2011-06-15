 \name{predict}

\docType{methods}

\alias{predict}
\alias{predict,Raster-method}

\title{Spatial model predictions}

\description{
Make a Raster* object with predictions based on a a fitted model object. The first argument is a Raster* object with the independent variables. The layerNames in the RasterStack should exactely match those expected by the model. This will be the case if the same Raster* object was used (via \code{extract}) to obtain the values to fit the model (see the example). Any type of model (e.g. glm. gam, randomForest) for which a predict method has been implemented (or can be implemented) can be used. 
}

\usage{
predict(object, ...) 
}


\arguments{
  \item{object}{a RasterStack, RasterBrick or RasterLayer object}
  \item{...}{Additional arguments. See under Methods}  
}

\section{Methods}{
\describe{
 
\code{predict(object, model, filename='', fun=predict, ext=NULL, const=NULL, index=1, na.rm=TRUE, ...)}
  
\tabular{rll}{  
  \tab  \code{object} \tab a Raster* object. Typcially a multi-layer type (RasterStack or RasterBrick) \cr
  \tab  \code{model}  \tab A fitted model of any class that has a 'predict' method (or for which you can supply a similar method as \code{fun} argument. E.g. glm, gam, or randomForest \cr
  \tab  \code{filename}  \tab Optional output filename \cr
  \tab  \code{fun} \tab Function. Default value is 'predict', but can be replaced with e.g. predict.se (depending on the type of model), or your
  own custom function.\cr
  \tab  \code{ext} \tab An Extent object to limit the prediction to a sub-region of \code{x} \cr
  \tab  \code{const}  \tab data.frame. Can be used to add a constant for which there is no Raster object for model predictions. Particulalry useful if the constant is a character-like factor value for which it is currently not possible to make a RasterLayer \cr
  \tab  \code{index}  \tab Integer. To select the column if predict.'model' returns a matrix with multiple columns \cr
  \tab  \code{na.rm}  \tab Logical. Remove cells with \code{NA} values in the predictors before solving the model (and return a \code{NA} value for those cells). This option prevents errors with models that cannot handle \code{NA} values. In most other cases this will not affect the output. An exception is when predicting with a boosted regression trees model because these return predicted value even if some (or all!) variables are \code{NA} \cr
  \tab  \code{...}  \tab Additional arguments to pass to the predict.'model' function \cr
}

The argument \code{se.fit} has been removed. See the examples on how to get a prediction of se.fit of a glm model.

The following additional arguments can be passed, to replace default values
\tabular{rll}{
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{progress} \tab Character. "text", "window", or "" (the default, no progress bar)  \cr
 }

}}


\seealso{
Use \code{\link[raster]{interpolate}} if your model has 'x' and 'y' as implicit independent variables (e.g., in kriging).
}

\value{
RasterLayer or RasterBrick
}

\author{ Robert J. Hijmans}

\examples{
# A simple model to predict the location of the R in the R-logo using 20 presence points 
# and 50 (random) pseudo-absence points. This type of model is often used to predict species distributions
# see the dismo package for more of that.

# create a RasterStack or RasterBrick with with a set of predictor layers
logo <- brick(system.file("external/rlogo.grd", package="raster"))
layerNames(logo)

\dontrun{
# the predictor variables
par(mfrow=c(2,2))
plotRGB(logo, main='logo')
plot(logo, 1, col=rgb(cbind(0:255,0,0), maxColorValue=255))
plot(logo, 2, col=rgb(cbind(0,0:255,0), maxColorValue=255))
plot(logo, 3, col=rgb(cbind(0,0,0:255), maxColorValue=255))
}

#get presence and pseudo-absence points
p <- matrix(c(48, 48, 48, 53, 50, 46, 54, 70, 84, 85, 74, 84, 95, 85, 66, 42, 26, 4, 19, 17, 7, 14, 26, 29, 39, 45, 51, 56, 46, 38, 31, 22, 34, 60, 70, 73, 63, 46, 43, 28), ncol=2)
# 
a <- cbind(runif(250)*(xmax(logo)-xmin(logo))+xmin(logo), runif(250)*(ymax(logo)-ymin(logo))+ymin(logo))

#extract values for points from stack
xy <- rbind(cbind(1, p), cbind(0, a))
v <- data.frame(cbind(xy[,1], extract(logo, xy[,2:3])))
colnames(v)[1] <- 'pa'

#build a model, here an example with glm 
model <- glm(formula=pa~., data=v)

#predict to a raster
r1 <- predict(logo, model, progress='window')

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

\dontrun{
require(randomForest)
rfmod <- randomForest(pa ~., data=v)

## note the additional argument "type='response'" that is passed to predict.randomForest
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
