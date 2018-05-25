[![Build Status](https://travis-ci.com/benja0x40/LittleThumb.svg?token=pShgRyyyZbvkbZAsmdMo&branch=master)](https://travis-ci.com/benja0x40/LittleThumb)
[![Coverage Status](https://codecov.io/gh/benja0x40/LittleThumb/branch/master/graph/badge.svg)](https://codecov.io/gh/benja0x40/LittleThumb)

LittleThumb
================================================================================

Automation mechanism for saving and loading R objects to/from RDS files.

### <a name="install"></a>Installation

Run the `R` code below to install `LittleThumb`.

```R
library("devtools") # (devtools can be installed from CRAN repositories)
install_github("benja0x40/LittleThumb", dependencies = T)
```

### <a name="basics"></a>Basic principles

An R script based on `LittleThumb` is structured by series of 3 consecutive
sections.

  1. Configuration of options using the `LittleThumb` function
  2. Definition of R objects using the `MakeObj` function
  3. Anything else depending on the defined R objects

However, a good practice is to use the configuration section only once,
at the begining of the script, such that any change in this section can
control all R objects subsequently defined with the `MakeObj` function.

Each time a script based on `LittleThumb` will be executed, objects being
defined by the `MakeObj` function will be automatically computed and saved
or loaded, depending on their availability in the R environment and at the
automatically saved locations, as well as the choosen global options.

And during each script execution, `LittleThumb` will show the status of
operations performed automatically for the defined objects.

Here is a minimalistic example of R script with a single object named `xyz`.

```R
# MiniScript.R

library(LittleThumb)

# 1. Configure global options --------------------------------------------------

# Here we choose the default location for automatically saved RDS files
LittleThumb(path = "AutoSaved")

# 2. Define persistent R objects -----------------------------------------------

MakeObj(xyz, {

  # Here we compute the value of this R object
  xyz <- 0

})

# 3. Do anything with defined R objects ----------------------------------------

print(xyz)
```

Let's run `MiniScript.R` for the very first time and see the resulting messages.

```R
source("MiniScript.R")
```

    [creating] AutoSaved
    [saving] AutoSaved/xyz.rds

These messages indicate that during this first execution of `MiniScript.R`,
the `AutoSaved` folder has been created in the current directory and 
the `xyz` object has been saved as `AutoSaved/xyz.rds`.

From now on, as long as the `xyz` object remains available in the R environment
and the global options are not changed, the `MakeObj(xyz, ...)` function call
will not do anything during repeated executions of `MiniScript.R`.

Let's run `MiniScript.R` a second time and see the resulting messages.

```R
source("MiniScript.R")
```

    [passing] AutoSaved/xyz.rds

If we execute the script once again but when `xyz` is no longer available,
for instance after restarting the R environment, the `MakeObj(xyz, ...)`
function call will automatically load `xyz` from the `AutoSaved/xyz.rds` file
instead of recomputing this object from scratch.

Let's simulate a loss of the object `xyz` due to restarting the R environment.

```R
rm(xyz)
```

And then let's run `MiniScript.R` once again to see the resulting messages.

```R
source("MiniScript.R")
```

    [loading] AutoSaved/xyz.rds

### Automation options

Work in progress.

### Advanced usage

See the vignette [LittleThumb::Documentation](inst/doc/Documentation.html)
for further documentation.

