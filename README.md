LittleThumb
================================================================================

Automation and traceability for sequencing data analysis with R/Bioconductor.

### Package installation ###

#### Prerequisites ####

  - [R environment](https://www.r-project.org/) version 3.x
  - CRAN packages `devtools`, `stringr`, `dplyr`, `jsonlite`, `optparse`
  - [Bioconductor](http://www.bioconductor.org/) packages
    `S4Vectors`, `GEOquery`, `SRAdb`, `GenomeInfoDb`
  
Run the R code below to install CRAN and Bioconductor package dependencies
for `LittleThumb`.

```R
# Already installed
pkg <- installed.packages()[, "Package"]

# CRAN packages
lst <- c("devtools", "lazyeval", "stringr", "dplyr", "jsonlite", "optparse", "igraph")
lst <- setdiff(lst, pkg)
if(length(lst) > 0) install.packages(lst, repos = "https://cloud.r-project.org/")

# Bioconductor packages
lst <- c("S4Vectors", "GEOquery", "SRAdb", "GenomeInfoDb")
lst <- setdiff(lst, pkg)
if(length(lst) > 0) {
  source("https://bioconductor.org/biocLite.R")
  biocLite(lst)
}
```

#### Installation from github ####

Run the bash code below to build package `LittleThumb` from github.

```bash
# Clone github repository
cd ~/DataImportTools
git clone git@github.com:benja0x40/LittleThumb.git

# Update cloned repository
cd ~/DataImportTools/LittleThumb
git pull

# Build package
cd ..
R CMD build LittleThumb
```

Run the R code below to install `LittleThumb`.

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

