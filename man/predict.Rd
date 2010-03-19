\name{predict}

\docType{methods}

\alias{predict}
\alias{predict,Raster-method}

\title{Spatial model predictions}

\description{
Make a RasterLayer with a prediction based on a a fitted model object. Provide a Raster* with the independent variables. The layerNames in the RasterStack should exactely match those expected by the model. This will be the case if the RasterStack was used (via \code{xyValues}) to obtain the values to fit the model (see the example). Any type of model (e.g. glm. gam, randomforest) for which a predict method has been implemented can be used. 
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
 
\code{predict(object, model, filename='', ext=NULL, const=NULL, index=1, se.fit=FALSE, ...)}
  
\tabular{rll}{  
  \tab  \code{object} \tab a RasterStack, RasterBrick or RasterLayer object \cr
  \tab  \code{model}  \tab A fitted model\cr
  \tab  \code{filename}  \tab Output filename for a new raster; if NA the result is not written to a file but returned with the RasterLayer object, in the data slot \cr
  \tab  \code{ext} \tab An Extent object to limit the prediction to a sub-region of \code{x} \cr
  \tab  \code{const}  \tab data.frame. Can be used to add a constant for which there is no Raster object for model predictions. Particulalry useful if the constant is a character-like factor value \cr
  \tab  \code{index}  \tab Integer. To select the column if predict.'model' returns a matrix with multiple columns \cr
  \tab  \code{se.fit}  \tab Extract the standard error rather then the prediction (works for GLM and perhaps other methods where the prediction and se.fit are returned as a list) \cr
  \tab  \code{...}  \tab Additional arguments to pass to the predict.'model' function \cr
}


The following additional arguments can be passed, to replace default values
\tabular{rll}{
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{progress} \tab Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""  \cr
 }

}}


\seealso{
Use \code{\link[raster]{interpolate}} if your model has 'x' and 'y' as implicit independent variables (e.g., in kriging).
}

\value{
a RasterLayer object
}

\author{ Robert J. Hijmans}

\examples{
# A simple model to predict the location of the R in the R-logo using 20 presence points 
# and 50 (random) pseudo-absence points. This type of model is often used to predict species distributions

# create a RasterStack (a set of predictor rasters)
logo <- stack(system.file("pictures/Rlogo.jpg", package="rgdal"))
layerNames(logo) <- c('red', 'green', 'blue')

#get presence and absence points
presence <- matrix(c(48, 48, 48, 53, 50, 46, 54, 70, 84, 85, 74, 84, 95, 85, 66, 42, 26, 4, 19, 17, 7, 14, 26, 29, 39, 45, 51, 56, 46, 38, 31, 22, 34, 60, 70, 73, 63, 46, 43, 28), ncol=2)
# random absence
absence <- cbind(runif(50)*(xmax(logo)-xmin(logo))+xmin(logo), runif(50)*(ymax(logo)-ymin(logo))+ymin(logo))

#extract values for points from stack
xy <- rbind(cbind(1, presence), cbind(0, absence))
v <- cbind(xy[,1], xyValues(logo, xy[,2:3]))
colnames(v)[1] <- 'presabs'

#build a model, here an example with glm 
model <- glm(formula=presabs~., data=data.frame(v))

#predict to a raster
r <- predict(logo, model, progress='text')

plot(r>0.3)
points(presence, bg='blue', pch=21)
points(absence, bg='red', pch=21)

## also try:
# require(randomForest)
## formula <- as.factor(presabs) ~.
# formula <- presabs ~.
# model <- randomForest(formula, data=data.frame(v))
# r2 <- predict(logo, type='response', model, progress='text')
## note the additional argument "type='response'" that is passed to predict.randomForest

}

\keyword{methods}
\keyword{spatial}
