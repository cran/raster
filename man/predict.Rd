\name{predict}

\docType{methods}

\alias{predict}
\alias{predict,Raster-method}

\title{Spatial model predictions}

\description{
Make a RasterLayer with a prediction based on a a fitted model object. Provide a Raster* with the independent variables. The layerNames in the RasterStack should exactely match those expected by the model. This will be the case if the RasterStack was used (via \code{extract}) to obtain the values to fit the model (see the example). Any type of model (e.g. glm. gam, randomforest) for which a predict method has been implemented can be used. 
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
 
\code{predict(object, model, filename='', fun=predict, ext=NULL, const=NULL, index=1, se.fit=FALSE, na.rm=TRUE, ...)}
  
\tabular{rll}{  
  \tab  \code{object} \tab a Raster* object. Typcially a multi-layer type (RasterStack or RasterBrick) \cr
  \tab  \code{model}  \tab A fitted model of any class that has a 'predict' method. E.g. glm, gam, or randomForest \cr
  \tab  \code{filename}  \tab Output filename for a new raster; if NA the result is not written to a file but returned with the RasterLayer object, in the data slot \cr
  \tab  \code{fun} \tab Function. Default value is 'predict', but can be replaced with e.g. predict.se (depending on the type of model)\cr
  \tab  \code{ext} \tab An Extent object to limit the prediction to a sub-region of \code{x} \cr
  \tab  \code{const}  \tab data.frame. Can be used to add a constant for which there is no Raster object for model predictions. Particulalry useful if the constant is a character-like factor value for which it is currently not possible to make a RasterLayer \cr
  \tab  \code{index}  \tab Integer. To select the column if predict.'model' returns a matrix with multiple columns \cr
  \tab  \code{se.fit}  \tab Compute the standard error AND the predicted value (works for GLM and perhaps other methods where the prediction and se.fit are returned as a list) \cr
  \tab  \code{na.rm}  \tab Logical. Remove cells with NA values in the predictors before solving the model (and return a NA value for those cells). This option prevents errors with models that cannot handle NA values. In most other cases this will not affect the output. An exception is when predicting with a boosted regression trees model because these return predicted value even if some (or all!) variables are NA \cr
  \tab  \code{...}  \tab Additional arguments to pass to the predict.'model' function \cr
}


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
RasterLayer
}

\author{ Robert J. Hijmans}

\examples{
# A simple model to predict the location of the R in the R-logo using 20 presence points 
# and 50 (random) pseudo-absence points. This type of model is often used to predict species distributions

# create a RasterStack (a set of predictor rasters)
logo <- stack(system.file("external/rlogo.grd", package="raster"))
layerNames(logo)

#par(mfrow=c(2,2))
#plotRGB(logo, main='logo')
#plot(logo, 1, col=rgb(cbind(0:255,0,0), maxColorValue=255))
#plot(logo, 2, col=rgb(cbind(0,0:255,0), maxColorValue=255))
#plot(logo, 3, col=rgb(cbind(0,0,0:255), maxColorValue=255))

#get presence and random background (pseudo-absence) points
presence <- matrix(c(48, 48, 48, 53, 50, 46, 54, 70, 84, 85, 74, 84, 95, 85, 66, 42, 26, 4, 19, 17, 7, 14, 26, 29, 39, 45, 51, 56, 46, 38, 31, 22, 34, 60, 70, 73, 63, 46, 43, 28), ncol=2)
# random background
background <- cbind(runif(250)*(xmax(logo)-xmin(logo))+xmin(logo), runif(250)*(ymax(logo)-ymin(logo))+ymin(logo))

#extract values for points from stack
xy <- rbind(cbind(1, presence), cbind(0, background))
v <- cbind(xy[,1], extract(logo, xy[,2:3]))
colnames(v)[1] <- 'presback'

#build a model, here an example with glm 
model <- glm(formula=presback~., data=data.frame(v))

#predict to a raster
r <- predict(logo, model, progress='text')

plot(r)
points(presence, bg='blue', pch=21)
points(background, bg='red', pch=21)

## also try:
# require(randomForest)
## formula <- as.factor(presback) ~.
# formula <- presback ~.
# model <- randomForest(formula, data=data.frame(v))
# r2 <- predict(logo, type='response', model, progress='text')
## note the additional argument "type='response'" that is passed to predict.randomForest
}

\keyword{methods}
\keyword{spatial}
