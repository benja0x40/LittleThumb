LittleThumb
================================================================================

### Examples ###

```bash
mkdir "ProjectFolder"
cd "ProjectFolder"
littlethumb import-basespace "ProjectName"
littlethumb map-reads -m bwa -x genomic_dm6,genomic_mm10 "_ANNOTATIONS_/import-basespace.ProjectName.LTID.txt"
littlethumb map-reads -t 8 -m bwa -x genomic_dm6,genomic_mm10 -f library_type=ChIP-Seq "_ANNOTATIONS_/import-basespace.ProjectName.LTID.txt"
```

```bash
mkdir "ProjectFolder"
cd "ProjectFolder"
littlethumb import-basespace "ProjectName"
littlethumb map-reads -t 8 -m bowtie -x genomic_dm6,genomic_mm10 "_ANNOTATIONS_/import-basespace.ProjectName.LTID.txt"
littlethumb map-reads -t 8 -m bwa -x genomic_dm6,genomic_mm10 "_ANNOTATIONS_/import-basespace.ProjectName.LTID.txt"
```


### Examples ###

```R
library(LittleThumb)
```

### Examples ###
```R

devtools::test()

openLittleThumb()

define_workspace("devel", "~/Documents/LT_TEST")
define_workspace("works", "~/Documents/LT_WORK")
list_workspaces()

use_workspace("devel")

list_workspaces()
```

