# Build the package:
library(devtools)
setwd("C:/Users/jlthomps/Desktop/git/USGS-NWC/R/RProjects/")
load_all("HITHATStats/",reset = TRUE)
setwd("C:/Users/jlthomps/Desktop/git/USGS-NWC/R/RProjects/HITHATStats")
document()
check()  
run_examples()
# test()   Assumes testthat type tests in GLRI/inst/tests
setwd("C:/Users/jlthomps/Desktop/git/USGS-NWC/R/RProjects/")
build("HITHATStats")
install("HITHATStats")

library(devtools)
setwd("C:/Users/jlthomps/Desktop/git/USGS-NWC/R/RProjects/")
load_all("NWCCompare/",reset = TRUE)
setwd("C:/Users/jlthomps/Desktop/git/USGS-NWC/R/RProjects/NWCCompare")
document()
check()  
run_examples()
# test()   Assumes testthat type tests in GLRI/inst/tests
setwd("C:/Users/jlthomps/Desktop/git/USGS-NWC/R/RProjects/")
build("NWCCompare")
install("NWCCompare")