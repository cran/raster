# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  October 2009
# Version 0.9
# Licence GPL v3

 
.writeHdrENVI <- function(raster) {
	hdrfile <- filename(raster)
	extension(hdrfile) <- ".hdr"
	thefile <- file(hdrfile, "w") 
	cat("ENVI\n", file = thefile)
	cat("description = {", raster@layernames, "}", "\n", file = thefile)
	cat("samples = ", ncol(raster), "\n", file = thefile)		
	cat("lines = ", nrow(raster), "\n", file = thefile)		
	cat("bands = ", raster@file@nbands, "\n", file = thefile)		
	cat("header offset = 0\n", file = thefile)		
	cat("file type = ENVI Standard\n", file = thefile)		
	dsize <- dataSize(raster@file@datanotation)
	if (.shortDataType(raster@file@datanotation) == 'INT') {
		if (dsize == 1) { dtype <- 1
		} else if (dsize == 2) { dtype <- 2
		} else if (dsize == 4) { dtype <- 3
		} else if (dsize == 8) { dtype <- 14
		} else { stop('what?')
		}
	} else {
		if (dsize == 4) { dtype <- 4
		} else if (dsize == 8) { dtype <- 5
		} else { stop('what?')
		}
	}	
	cat("data type = ", dtype, "\n", file = thefile)
#1=8-bit byte; 2=16-bit signed integer; 3=32-bit signed long integer; 4=32-bit floating point; 
#5=64-bit double-precision floating point; 6=2x32-bit complex, real-imaginary pair of double precision;
#9=2x64-bit double-precision complex, real-imaginary pair of double precision; 12=16-bit unsigned integer; 
#13=32-bit unsigned long integer; 14=64-bit signed long integer; and 15=64-bit unsigned long integer.

	cat("interleave = bil\n", file = thefile)	
	cat("sensor type = \n", file = thefile)		
	if (.Platform$endian == "little") { btorder <- 0 
	} else { btorder <- 1 }
	cat("byte order = ", btorder, "\n",file = thefile)		

	cat("map info = {projection, 1, 1,", xmin(raster),", ", ymax(raster),", ", xres(raster),", ", yres(raster), "}\n", file = thefile)
    cat("projection info =", projection(raster), "\n", file = thefile) 
	cat("z plot range = {", minValue(raster),", ", maxValue(raster), "}\n", file = thefile) 
	close(thefile)	
}

