# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  May 2009
# Version 0.9
# Licence GPL v3

.defaultExtension <- function(format=.filetype()) {
	if (format == 'raster') { return('.grd') 
	} else if (format == 'ascii') { return('.asc')
	} else if (format == 'CDF') { return('.nc')
	} else if (format == 'BIL') { return('.bil')
	} else if (format == 'BSQ') { return('.bsq')
	} else if (format == 'BIP') { return('.bip')
	} else if (format == 'BMP') { return('.bmp') 
	} else if (format == 'ADRG') { return('.gen') 
	} else if (format == 'BT') { return('.bt') 
	} else if (format == 'EHdr') { return('.bil')
	} else if (format == 'ENVI') { return('.envi')
	} else if (format == 'ERS') { return('.ers') 
	} else if (format == 'GSBG') { return('.grd')
	} else if (format == 'GTiff') { return('.tif') 
	} else if (format == 'HFA') { return( '.img') 
	} else if (format == 'IDA') { return( '.img') 
	} else if (format == 'RST') { return('.rst') 
	} else if (format == 'ILWIS') { return('.mpr')
	} else if (format == 'SAGA') { return('.sdat')
	} else if (format == 'RMF') { return('.rsw')
	} else { return('') }
}


.getExtension <- function(f, format) {
	if (.setfileext()) {
		def <- .defaultExtension(format)
		if (def != '') {
			ext(f) <- def
		}
	}
	return(f)
}

