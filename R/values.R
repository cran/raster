# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  October 2010
# Version 1.0
# Licence GPL v3
	
if (!isGeneric("values")) { 
	setGeneric("values", function(x, ...)
		standardGeneric("values"))
}	

setMethod('values', signature(x='Raster'), 
function(x, ...) {
	getValues(x, ...)
})
	
	
if (!isGeneric('values<-')) {
	setGeneric('values<-', function(x, layer, value)
		standardGeneric('values<-')) 
	}	

setMethod('values<-', signature(x='RasterLayer'), 
function(x, layer=-1, value) {
	setValues(x, value)
} )
	
setMethod('values<-', signature(x='RasterBrick'), 
function(x, layer=-1, value) {
	setValues(x, value, layer=layer)
} )
	
