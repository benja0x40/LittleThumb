[![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Build Status](https://travis-ci.com/benja0x40/LittleThumb.svg?branch=master)](https://travis-ci.com/benja0x40/LittleThumb)
[![Coverage Status](https://codecov.io/gh/benja0x40/LittleThumb/branch/master/graph/badge.svg)](https://codecov.io/gh/benja0x40/LittleThumb)

LittleThumb
================================================================================

LittleThumb is an R package under development which provides a lightweight
persistence mechanism for R objects, with the main motivation being to simplify
the storage and organization of results produced during the development
of analysis scripts or Rmarkdown notebooks.

### <a name="install"></a>Functionalities

 - transparent save/load operations to/from RDS files
 - global or individual control of storage locations and object updates
 - status messages indicating automatically performed operations


### <a name="install"></a>Installation

```R
# Package devtools can be installed from CRAN repositories
devtools::install_github("benja0x40/LittleThumb")
```


### <a name="example"></a>Example

The script below shows a minimalistic example defining a single persistent object
named `xyz`.

```R
# MiniScript.R
library(LittleThumb)

# 1. Configure automation options ----------------------------------------------
# Here we choose the default location for automatically saved RDS files
LittleThumb(rootpath = "AutoSaved")

# 2. Define a persistent R object ----------------------------------------------
MakeObj(xyz, {

  # Here we compute the value of object xyz
  xyz <- 0

})

# 3. Use this R object ---------------------------------------------------------
print(xyz)
```

When this example is run for the first time, the `AutoSaved` folder is
created and the `xyz` object is saved as `AutoSaved/xyz.rds`.

```R
source("MiniScript.R") # First execution
```

    [LittleThumb] create | AutoSaved
    [LittleThumb] save | xyz => AutoSaved/xyz.rds

From the second execution, as long as the `xyz` object remains available
in the R environment and LittleThumb automation options remain unchanged,
the `MakeObj(xyz, ...)` function call in `MiniScript.R` bypasses any execution.

```R
source("MiniScript.R") # Second execution
```

    [LittleThumb] bypass | xyz = AutoSaved/xyz.rds

Executing `MiniScript.R` once again but when the object `xyz` is no longer
available, for instance after restarting the R environment,
the `MakeObj` function call now automatically loads `xyz` from
the `AutoSaved/xyz.rds` file instead of recomputing this object.

```R
rm(xyz) # Simulate restarting the R environment

source("MiniScript.R") # Another execution (xyz being unavailable in R)
```

    [LittleThumb] load | xyz <= AutoSaved/xyz.rds


### Persistent R objects

The `MakeObj` function serves to define persistent objects and encapsulates
the block of code necessary to generate such object from scratch.

Each time a portion of R code including a `MakeObj` function call is executed,
the object defined using this function is automatically generated and saved,
or loaded, according to current automation options and depending on the object
availability in the current R environment and at its storage location.


### Automation options

The `LittleThumb` function provides access to options controlling
storage locations and object updates, either globally or individually
for each object defined with `MakeObj`.
The main options are `rootpath` which specifies the root location of RDS
files managed by LittleThumb, as well as `reload` and `rebuild`.

The effect of the `reload` and `rebuild` options in the context of the
`MiniScript.R` example is shown hereafter.

#### Reloading objects

The `reload` option allows to force reloading objects from their associated 
RDS file even when these objects are already available in the R environment.

```R
LittleThumb(reload = TRUE)  # All persistent objects must be reloaded
LittleThumb(reload = "xyz") # Only object xyz must be reloaded

source("MiniScript.R")
```

    [LittleThumb] reload | xyz <= AutoSaved/xyz.rds


#### Updating objects

The `rebuild` option controls whether objects should be recomputed and saved
even if the associated RDS file already exists at the expected location.

```R
LittleThumb(rebuild = TRUE)  # All persistent objects must be updated
LittleThumb(rebuild = "xyz") # Only object xyz must be updated

source("MiniScript.R")
```

    [LittleThumb] update | xyz => AutoSaved/xyz.rds


### Work in progress

These basic features of LittleThumb should remain stable in future versions,
which is not guaranteed for the other functionalities and options documented
in the package manual.

### Author

Benjamin Leblanc |
[GitHub](https://github.com/benja0x40) -
[ResearchGate](https://www.researchgate.net/profile/Benjamin_Leblanc) -
[LinkedIn](https://www.linkedin.com/in/benja0x40)
