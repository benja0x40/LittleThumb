[![Build Status](https://travis-ci.com/benja0x40/LittleThumb.svg?token=pShgRyyyZbvkbZAsmdMo&branch=master)](https://travis-ci.com/benja0x40/LittleThumb)
[![Coverage Status](https://codecov.io/gh/benja0x40/LittleThumb/branch/master/graph/badge.svg)](https://codecov.io/gh/benja0x40/LittleThumb)

LittleThumb (alpha)
================================================================================

LittleThumb is an R package whose main purpose is to simplify the storage and
organization of results produced by standalone data analysis scripts or
notebooks, from the stage of early prototypes to mature versions.
To do so, this package provides a lightweight persistence mechanism
for R objects, automating save/load operations to/from RDS files and allowing
global or individual control of object locations and update requirements.

### <a name="install"></a>Installation

Run the `R` code below to install `LittleThumb`.

```R
library("devtools") # (devtools can be installed from CRAN repositories)
install_github("benja0x40/LittleThumb")
```

### <a name="basics"></a>Basic principles

The two key functions achieving automated persitence of R objects are
`LittleThumb` and `MakeObj`.

The `MakeObj` function serves to define which objects should be persistent
and encapsulates the block of code responsible for generating such objects from
scratch.
Each time a `MakeObj` function is executed, the object it defines is
automatically generated and saved, or loaded, depending on the object
availability in the R environment and at its storage location, and
in accordance with automation options defined by the `LittleThumb` function.

Thus, from the point of view of these functionalities, an R script or notebook
can be seen as series of 3 consecutive sections.

  1. Configuration of automation options using `LittleThumb`
  2. Definition of R objects with `MakeObj`
  3. Anything else depending on the defined objects
     (for instance plot sections)

A recommended practice is to use only one configuration section, positionned
at the begining of the script or notebook, such that any change in this section
can affect all objects subsequently defined by `MakeObj`.

### <a name="example"></a>Example

During each script or notebook execution, LittleThumb produces messages
indicating the status of operations performed automatically.
The `MiniScript.R` example below shows a minimalistic R script defining a
single persistent object named `xyz` and is followed by status messages
obtained after running this script repeatedly without modification.

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

When `MiniScript.R` is run for the first time, status messages indicate
that the `AutoSaved` folder is created in the current directory
and that the `xyz` object is saved as `AutoSaved/xyz.rds`.

```R
source("MiniScript.R") # First execution
```

    [LittleThumb] create | AutoSaved
    [LittleThumb] save | xyz = AutoSaved/xyz.rds

From the second execution, as long as the `xyz` object remains available
in the R environment and the automation options remain unchanged,
the `MakeObj(xyz, ...)` function call in `MiniScript.R` bypasses operations
related to `xyz`.

```R
source("MiniScript.R") # Second execution
```

    [LittleThumb] bypass | xyz = AutoSaved/xyz.rds

Executing `MiniScript.R` once again but when the object `xyz` is no longer
available, for instance after restarting the R environment,
`MakeObj(xyz, ...)` now automatically loads `xyz` from the `AutoSaved/xyz.rds`
file instead of recomputing this object.

```R
# Simulate the loss of object xyz due to restarting the R environment
rm(xyz)

source("MiniScript.R") # Another execution (xyz being unavailable in R)
```

    [LittleThumb] load | xyz = AutoSaved/xyz.rds

### Automation options

The `LittleThumb` function provides access to options controlling
storage locations and automation behaviors, either globally or individually
for each object defined with `MakeObj`.

Most automation options are also accessible as arguments of the `MakeObj`
function. This allows for instance to lock specific options for specific
objects since the value of an optional `MakeObj` argument overrides the
corresponding value set by `LittleThumb`.

The most basic options are `rootpath` which defines the root location of RDS
files automatically saved and loaded by LittleThumb,
as well as `reload` and `rebuild`.
The effect of these two  options in the context of the `MiniScript.R` example
is shown hereafter.

#### Reloading objects

The `reload` option allows to force reloading objects from their associated 
RDS file even when these objects are already available in the R environment.

```R
LittleThumb(reload = "xyz") # Object xyz must be reloaded

source("MiniScript.R")
```

    [LittleThumb] reload | xyz = AutoSaved/xyz.rds


#### Rebuilding objects

The `rebuild` option controls whether objects should be regenerated and saved
when their associated RDS file already exists at its expected location.

```R
LittleThumb(rebuild = "xyz") # Object xyz must be recomputed and saved

source("MiniScript.R")
```

    [LittleThumb] overwrite | xyz = AutoSaved/xyz.rds


### Advanced usage

Advanced functionalities are not yet available.

### Credit and feedback

Benjamin Leblanc, 2018
([GitHub](https://github.com/benja0x40/LittleThumb) |
[ResearchGate](https://www.researchgate.net/profile/Benjamin_Leblanc))