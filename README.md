LittleThumb
================================================================================

Automation and traceability for sequencing data analysis with R/Bioconductor.

### Package installation ###

#### Prerequisites ####

  - [R environment](https://www.r-project.org/) version 3.x
  - R packages `devtools`, `S4Vectors`, `stringr`, `dplyr`, `jsonlite`, `optparse`
  - [Bioconductor](http://www.bioconductor.org/) packages `GEOquery`, `SRAdb`, `GenomeInfoDb`
  
The code below installs R and Bioconductor packages required by LittleThumb.

```R
# Already installed
pkg <- installed.packages()[, "Package"]

# CRAN packages
lst <- c("devtools", "S4Vectors", "stringr", "dplyr", "jsonlite", "optparse")
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

#### Installation from github ####

In the terminal.

```bash
# Clone github repository
cd ~/DataImportTools
git clone git@github.com:benja0x40/LittleThumb.git

# Update cloned repository
cd ~/DataImportTools/LittleThumb
git pull

# Build package
R CMD build LittleThumb
```

In the R environment.

```r
# When package will be public
# library("devtools")
# install_github("benja0x40/LittleThumb")

# Using manually built package archive
install.packages("LittleThumb_0.1.0.tar.gz")

# Post installation
library(LittleThumb)
installLittleThumb()
```

