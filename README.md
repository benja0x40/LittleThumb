[![Build Status](https://travis-ci.com/benja0x40/LittleThumb.svg?token=pShgRyyyZbvkbZAsmdMo&branch=master)](https://travis-ci.com/benja0x40/LittleThumb)
[![Coverage Status](https://codecov.io/gh/benja0x40/LittleThumb/branch/master/graph/badge.svg)](https://codecov.io/gh/benja0x40/LittleThumb)

LittleThumb (alpha)
================================================================================

LittleThumb is an R package providing a lightweight persistence mechanism for
R objects in order to simplify the storage and organization of results produced
by standalone data analysis scripts or notebooks.

### <a name="install"></a>Installation

Run the `R` code below to install `LittleThumb`.

```R
library("devtools") # (devtools can be installed from CRAN repositories)
install_github("benja0x40/LittleThumb")
```

### <a name="basics"></a>Description

The persistence mechanism consists in automated save/load operations to/from
RDS files at each execution of an R script using the 

thanks to the declarative functions
`MakeObj` and `LittleThumb`.

The `MakeObj` function serves to define which objects should be persistent
and encapsulates the block of code responsible for generating such objects from
scratch. The `LittleThumb` function serves to define automation options
which provide global or individual control of object locations and update
requirements.

Practically, each time a `MakeObj` function is executed, the object it defines
is automatically generated and saved, or loaded, depending on the object
availability in the R environment and at its storage location, and
in accordance with current automation options defined.

### Author

Benjamin Leblanc |
[GitHub](https://github.com/benja0x40) -
[ResearchGate](https://www.researchgate.net/profile/Benjamin_Leblanc)
