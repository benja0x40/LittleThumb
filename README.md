LittleThumb
================================================================================

Traceable importation and processing of sequencing data.

### Installation ###

#### Prerequisites ####

  - R environment version 3.x
  - R packages: `devtools`, `stringr`, `optparse`, `dplyr`
  - [Bioconductor](http://www.bioconductor.org/) packages: `GEOquery`, `SRAdb`, `GenomeInfoDb`
  
#### Installing dependencies ####

The code below installs R and Bioconductor packages required by LittleThumb.

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

#### Installing LittleThumb ####

In the terminal.

```bash
# Clone github repository
cd ~/DataImportTools
git clone git@github.com:benja0x40/LittleThumb.git
# Build package
R CMD build LittleThumb
```

In the R environment.

```r
# Using manually built package archive
install.packages("LittleThumb_0.1.0.tar.gz")

# When package devtools will be fixed (bug in current version 1.12.0)
library("devtools")
install_github("benja0x40/LittleThumb")

# Post installation
library(LittleThumb)
installLittleThumb()
```

