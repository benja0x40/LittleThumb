LittleThumb -  Traceable importation and processing of sequencing data
================================================================================

### Installation ###

#### Prerequisites ####

  - R environment version 3.x
  - R packages: `devtools`, `stringr`, `optparse`, `dplyr`
  - [Bioconductor](http://www.bioconductor.org/) packages: `GEOquery`, `SRAdb`, `GenomeInfoDb`
  
#### Installing dependencies ####

The script below installs R and Bioconductor packages required by LittleThumb.

```R
# Already installed
pkg <- installed.packages()[, "Package"]

# CRAN packages
lst <- c("devtools", "stringr", "optparse", "dplyr")
lst <- setdiff(lst, pkg)
if(length(lst) > 0) install.packages(lst, repos = "https://cloud.r-project.org/")

# Bioconductor packages
lst <- c("GEOquery", "SRAdb", "GenomeInfoDb")
lst <- setdiff(lst, pkg)
if(length(lst) > 0) {
  source("https://bioconductor.org/biocLite.R")
  biocLite(lst)
}
```

#### Installing MRA.TA ####

```r
library("devtools")
install_github("benja0x40/LittleThumb")
```

