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
  packageStartupMessage("This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information. Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.")
}