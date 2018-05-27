[![Build Status](https://travis-ci.com/benja0x40/LittleThumb.svg?token=pShgRyyyZbvkbZAsmdMo&branch=master)](https://travis-ci.com/benja0x40/LittleThumb)
[![Coverage Status](https://codecov.io/gh/benja0x40/LittleThumb/branch/master/graph/badge.svg)](https://codecov.io/gh/benja0x40/LittleThumb)

LittleThumb
================================================================================

LittleThumb is an R package whose main purpose is to simplify the storage and
organization of intermediate results during the development of scripts
or notebooks involving heterogeneous and time consuming data importation
or processing tasks.
To achieve this, LittleThumb provides a lightweight persistence mechanism for
R objects which automates save/load operations to/from RDS files and allows
global or individual control of locations and update requirements.

### <a name="install"></a>Installation

Run the `R` code below to install `LittleThumb`.

```R
library("devtools") # (devtools can be installed from CRAN repositories)
install_github("benja0x40/LittleThumb")
```

### <a name="basics"></a>Basic principles

An R script or notebook based on LittleThumb is structured by series of 3
consecutive sections.

  1. Configuration of global options using the `LittleThumb` function
  2. Definition of R objects using the `MakeObj` function
  3. Anything else depending on the defined R objects

A recommended practice is to use the configuration section only once, 
at the begining of the script or notebook, such that any change in this section
can affect all R objects subsequently defined with the `MakeObj` function.

Each time any R code including the `MakeObj` function is executed, the objects
defined using this function are automatically generated and saved, or loaded,
according to current options and depending on objects availability in the R
environment and at their storage location.

During these executions, LittleThumb produces messages indicating the status
of operations performed automatically for the corresponding objects.

Here is a minimalistic example of R script defining a single persistent object
named `xyz`.

```R
# MiniScript.R

library(LittleThumb)

# 1. Configure global options --------------------------------------------------

# Here we choose the default location for automatically saved RDS files
LittleThumb(rootpath = "AutoSaved")

# 2. Define persistent R objects -----------------------------------------------

MakeObj(xyz, {

  # Here we compute the value of the object
  xyz <- 0

})

# 3. Do anything with defined R objects ----------------------------------------

print(xyz)
```

When `MiniScript.R` is run for the first time, LittleThumb's messages indicate
that the `AutoSaved` folder has been created in the current directory
and that the `xyz` object has been saved as `AutoSaved/xyz.rds`.

```R
source("MiniScript.R")
```

    [LittleThumb] create | AutoSaved
    [LittleThumb] save | xyz = AutoSaved/xyz.rds

From the second execution, as long as the `xyz` object remains available
in the R environment and the options remain unchanged, the `MakeObj(xyz, ...)`
function call in `MiniScript.R` bypasses operations related to `xyz`.

```R
source("MiniScript.R")
```

    [LittleThumb] bypass | xyz = AutoSaved/xyz.rds

Executing `MiniScript.R` once again but when the object `xyz` is no longer
available, for instance after restarting the R environment,
the `MakeObj(xyz, ...)` function automatically loads `xyz` from
the `AutoSaved/xyz.rds` file instead of recomputing this object from scratch.

```R
# Simulate a loss of the object `xyz` due to restarting the R environment
rm(xyz)

source("MiniScript.R")
```

    [LittleThumb] load | xyz = AutoSaved/xyz.rds

### Automation options

All automation options are available as argument of the `MakeObj` function to
control each object individually and can also be modified globally using
the `LittleThumb` function.

For instance, two basic options are `overload` and `rebuild`.
The `overload` option allows to force loading objects from their associated 
RDS file even when these objects are already available in the R environment.
The `rebuild` option controls whether objects should be regenerated and saved
when their associated RDS file already exists at its expected location.

### Advanced usage

See the built-in vignette and manual for further documentation.

```R
vignette("Documentation", package = "LittleThumb")
```

### Alternatives

