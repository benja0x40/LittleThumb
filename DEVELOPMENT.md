LittleThumb
================================================================================

<div style='color:red'>This is some red text.</div>

Automation and traceability for sequencing data analysis with R/Bioconductor.

### Core concepts ###

LittleThumb organizes data analysis projects into workspaces, datasets and jobs,
providing both command line (bash) and R front ends.

#### Configurations ####

LittleThumb uses configuration levels 

Default settings are inherited  
package -> host -> user -> workspace -> job

Specified settings overwrite default ones  
job -> workspace -> user -> host -> package

**Package configuration**


#### Workspaces ####

A workspace is a container for data analyses. It provides a root location for
storage of imported and processed data where LittleThumb can ensure the
traceability and consistency of associated jobs and datasets.

| Function        | Command         | Description                              |
| --------------- | --------------- | ---------------------------------------- |
| list_workspaces | list-workspaces | list available workspaces                |
| load_workspace  |                 | load workspace (R)                       |
| save_workspace  |                 | save workspace (R)                       |
| list_datasets   | list-datasets   | list available datasets                  |
| load_dataset    |                 | load dataset (R)                         |
| save_dataset    |                 | save dataset (R)                         |

| Attribute      | Description                                                 |
| -------------- | ----------------------------------------------------------- |
| `lt_id`        | unique identifier -> default label for files and R objects  |
| `name`         | user defined name -> default label if specified + valid     |
| `path`         | file system folder (root of the workspace)                  |
| `cfg`          | list of config options                                      |
| `dts_register` | datasets register                                           |
| `jbs_register` | jobs register                                               |

**Configuration**

| Option         | Default | Description                                       |
| -------------- | ------- | ------------------------------------------------- |
| `CFGDIR`       | inherit | config files path (relative to workspace)         |
| `DTSDIR`       | inherit | datasets metadata path (relative to workspace)    |
| `JBSDIR`       | inherit | jobs metadata path (relative to workspace)        |



**Datasets register**

| Attribute       | Description                                                 |
| --------------- | ----------------------------------------------------------- |
| `lt_id`         | unique identifier -> default label for files and R objects  |
| `name`          | user defined name -> default label if specified + valid     |
| `primary`       | TRUE if not derived from pre-existing dataset               |
| `src_workspace` | source workspace identifier (NA if none)                    |
| `src_dataset`   | source dataset identifier (NA if none)                      |
| `src_job`       | source job identifier                                       |
| `path`          | location of metadata (relative to workspace)                |
| `file`          | metadata file name (relative to path)                       |
| `data_type`     | accepted file formats (fastq, bam, bed, etc.)               |
| `data_path`     | location of actual data (relative to workspace)             |

**Jobs register**

| Attribute      | Description                                                 |
| -------------- | ----------------------------------------------------------- |
| `lt_id`        | unique identifier -> default label for files and R objects  |
| `name`         | user defined name -> default label if specified + valid     |


#### Datasets ####

A dataset is a collection of data sharing the same storage location as well as
the same storage format, and describded by the same table of metadata.

For instance, datasets can represent any collection of fatsq or bam files
associated with sequencing experiments, or read counts over genomic intervals,
or bed files with coordinates of ChIP-enriched regions, or bigwig files of
genomic profiles.

| Attribute      | Description                                                 |
| -------------- | ----------------------------------------------------------- |
| lt_id            | unique identifier -> default label for files and R objects |
| name             | user defined name -> default label if specified + valid    |
| source           | workspace, dataset, job identifier                         |
| description_file | name of the dataset description file                       |

| Function        | Command         | Description                              |
| --------------- | --------------- | ---------------------------------------- |
| list_workspaces | list-workspaces | list available workspaces                |
| load_workspace  |                 | load workspace (R)                       |
| save_workspace  |                 | save workspace (R)                       |
| list_datasets   | list-datasets   | list available datasets                  |
| load_dataset    |                 | load dataset (R)                         |
| save_dataset    |                 | save dataset (R)                         |

**Implementation**

Dataset metatdata must be accessed and updated easily either by editing the
associated text file or by modifying the associated R object.

Use hybrid file structure with 2 sections.
Section 1: commented lines with JSON formatted data
Section 2: tab delimited table with header column names

I/O functions:
Use readLines, split sections 1 and 2 (comment char), parse JSON, parse table,
join into object




**Dynamic loading/saving of actual data**


#### Jobs ####

Jobs represent executable tasks on datasets.

| Attribute      | Description                                                |
| -------------- | ---------------------------------------------------------- |
| id             | unique identifier                                          |
| production     | host computer, user, working dir, execution mode (bash, R) |
| configuration  | versions, system level settings and default parameters     |
| action         | command call, function name                                |
| parameters     |                                                            |
| source         | workspace, dataset, input folder                           |
| destination    | workspace, dataset, output folder                          |

#### Datasets ####

A dataset is a collection of data sharing the same storage location as well as
the same storage format, and describded by the same table of metadata.

For instance, datasets can represent any collection of fatsq or bam files associated with sequencing experiments, or read counts over genomic intervals,
or bed files with coordinates of ChIP-enriched regions, or bigwig files of
genomic profiles.

#### Jobs ####


### Package installation ###

Source package structure

- **LittleThumb**
    - **R**  
    - **man**  
    - **exec**  
        contains the R script executing `littlethumb` command calls
    - **inst**  
        - **config**  
            default configuration files
        - **modules**  
            source code for each module

Binary/installed package structure

- **LittleThumb**
    - **R**  
    - **man**  
    - **exec**  
    - **config**  
    - **modules**  


### Configuration ###


