EflowStats
===========

Calculates a suite of ecological flow statistics and fundamental properties of daily streamflow for a given set of data. 

The `EflowStats` package was created to simplify the process of generating hydrologic indicator statistics using daily streamflow records. It has been specifically designed to work seamlessly with U.S. Geological Survey (USGS) National Water Information System (NWIS) data. This package is intended to be an update of the previously existing USGS National Hydrologic Assessment Tool (NAHAT) program with additional statistics previously published by Archfield et al (Archfield).


Installation
----------
To install this package use the following code:

```r
install.packages("EflowStats",repos=c("http://owi.usgs.gov/R","http://cran.us.r-project.org"))
```

Linux Tests: [![travis](https://travis-ci.org/USGS-R/EflowStats.svg?branch=master)](https://travis-ci.org/USGS-R/EflowStats)

Windows Tests: [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/USGS-R/EflowStats?branch=master&svg=true)](https://ci.appveyor.com/project/USGS-R/EflowStats)

Code Coverage: [![Coverage Status](https://coveralls.io/repos/github/USGS-R/EflowStats/badge.svg?branch=master)](https://coveralls.io/github/USGS-R/EflowStats?branch=master)


References
----------

Henriksen, J.A., Heasley, J. Kennen, J.G., and Nieswand, S., 2006, Users' manual for the Hydroecological Integrity Assessment Process software (including the New Jersey Assessment Tools): U.S. Geological Survey Open-File Report 2006-1093. 72 p. ([http://www.fort.usgs.gov/products/publications/21598/21598.pdf](http://www.fort.usgs.gov/products/publications/21598/21598.pdf))

Archfield, S.A., J.G. Kennen, D.M. Carlisle, and D.M. Wolock. 2013. An Objective and Parsimonious Approach for Classifying Natural Flow Regimes at a Continental Scale. River Res. Applic. doi: 10.1002/rra.2710 ([http://onlinelibrary.wiley.com/doi/10.1002/rra.2710/abstract](http://onlinelibrary.wiley.com/doi/10.1002/rra.2710/abstract))

Disclaimer
----------
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [http://www.usgs.gov/visual-id/credit_usgs.html#copyright](http://www.usgs.gov/visual-id/credit_usgs.html#copyright)


Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."


 [
    ![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)
  ](http://creativecommons.org/publicdomain/zero/1.0/)
