#' This is data to be included in my package
#'
#' @name obs_data
#' @docType data
#' @author Jessica Thompson \email{jlthomps@@usgs.gov}
#' @keywords data
NULL

#' Example data
#'
#' Example data 
#'
#' @name sampleData
#' @docType data
#' @keywords streamflow data
NULL

#' This is data to be included in my package
#'
#' @name qfiletempf
#' @docType data
#' @author Jessica Thompson \email{jlthomps@@usgs.gov}
#' @keywords data
NULL

#' This is data to be included in my package
#'
#' @name dailyData
#' @docType data
#' @author Jessica Thompson \email{jlthomps@@usgs.gov}
#' @keywords data
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.")
}