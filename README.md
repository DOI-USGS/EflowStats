# `EflowStats`

Calculates a suite of ecological flow statistics and fundamental properties of daily streamflow for a given set of data. 

|Linux|Windows| Test Coverage | USGS Status |
|----------|------------|------------|------------|
| [![travis](https://travis-ci.org/USGS-R/EflowStats.svg?branch=master)](https://travis-ci.org/USGS-R/EflowStats) | [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/USGS-R/EflowStats?branch=master&svg=true)](https://ci.appveyor.com/project/USGS-R/EflowStats) | [![Coverage Status](https://coveralls.io/repos/github/USGS-R/EflowStats/badge.svg?branch=master)](https://coveralls.io/github/USGS-R/EflowStats?branch=master) | [![status](https://img.shields.io/badge/USGS-Research-blue.svg)](https://owi.usgs.gov/R/packages.html#research) |

## !!!NOTICE!!!
There have been major revisions to `EflowStats` since version 4.1.1! [Review the intro vignette](https://cdn.rawgit.com/USGS-R/EflowStats/9507f714/inst/doc/intro.html) for how to use the package and a brief description of changes [in this vignette.](https://cdn.rawgit.com/USGS-R/EflowStats/707bec71/inst/doc/packageDiscrepencies.html)

### Installation

To install the latest release of EflowStats see [releases](https://github.com/USGS-R/EflowStats/releases) to find the latest version and use:
```r
remotes::install_github("USGS-R/EflowStats@v5.0.1")
```

To install the latest and greatest build use the following code:

```r
remotes::install_github("USGS-R/EflowStats")
```

### Code of Conduct

We want to encourage a warm, welcoming, and safe environment for contributing to this project. See the [code of conduct](https://github.com/USGS-R/EflowStats/blob/master/CONDUCT.md) for more information.

### Reporting bugs

Please consider reporting bugs and asking questions on the [Issues page.](https://github.com/USGS-R/EflowStats/issues)

### Package Support

The Water Mission Area of the USGS has supported the development and maintenance of the `EflowStats` R-package. Further maintenance is expected to be stable until further notice. Resources are available primarily for maintenance and responding to user questions. Priorities on the development of new features are determined by the `EflowStats` development team.

![USGS](http://usgs-r.github.io/images/usgs.png)

### Overview

The `EflowStats` package was created to simplify the process of generating hydrologic indicator statistics using daily streamflow records. It has been specifically designed to work seamlessly with U.S. Geological Survey (USGS) National Water Information System (NWIS) data. This package is intended to be an update of the previously existing USGS National Hydrologic Assessment Tool (NAHAT) program with additional statistics previously published by Archfield et al (Archfield).

### References

Henriksen, J.A., Heasley, J. Kennen, J.G., and Nieswand, S., 2006, Users' manual for the Hydroecological Integrity Assessment Process software (including the New Jersey Assessment Tools): U.S. Geological Survey Open-File Report 2006-1093. 72 p. ([http://www.fort.usgs.gov/products/publications/21598/21598.pdf](http://www.fort.usgs.gov/products/publications/21598/21598.pdf))

Archfield, S.A., J.G. Kennen, D.M. Carlisle, and D.M. Wolock. 2013. An Objective and Parsimonious Approach for Classifying Natural Flow Regimes at a Continental Scale. River Res. Applic. doi: 10.1002/rra.2710 ([http://onlinelibrary.wiley.com/doi/10.1002/rra.2710/abstract](http://onlinelibrary.wiley.com/doi/10.1002/rra.2710/abstract))

Disclaimer
----------
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [http://www.usgs.gov/visual-id/credit_usgs.html#copyright](http://www.usgs.gov/visual-id/credit_usgs.html#copyright)


This software has been approved for release by the U.S. Geological Survey (USGS). Although the software has been subjected to rigorous review, the USGS reserves the right to update the software as needed pursuant to further analysis and review. No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. Furthermore, the software is released on condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from its authorized or unauthorized use.


 [
    ![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)
  ](http://creativecommons.org/publicdomain/zero/1.0/)
