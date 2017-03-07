LittleThumb
================================================================================

Automation and traceability for sequencing data analysis with R/Bioconductor.

### **A. Key concepts** ###

Genomic research projects involve complex and often time consuming bioinformatic activities.

From a high level perspective these activities can be grouped in 3 main
categories.
The first category consist in retrieval and organization of sequencing data
coming from ongoing experiments as well as from previously published studies.
The second category consist in performing data analysis and can often involve
exploratory and design/development activities.
The third category is production of publication quality figures and associated
documentation (e.g. legends, material and methods, etc.). 

The LittleThumb project aims to provide a solution connecting as transparently
as possible these 3 group of activities for genomic research projects involving
numerous datasets, extensive data exploration and advanced data analyses.

To achieve this aim LittleThumb organizes genomic research projects into
workspaces, datasets, jobs and productions, proposing two interfaces, one
accessible via terminal commands (bash), and one accessible via the
R/Bioconductor environment, that operate consistently with each other.

#### **1. Workspaces** ####

A workspace is a container for data analyses. It provides a root location for
storage of imported and processed data where LittleThumb can ensure the
traceability and consistency of associated jobs and datasets.

For the moment Workspaces are independent from each other, meaning that datasets
cannot be shared between workspaces. Sharing need to adress indirect data
access plus maintenance of reverse dependency and locking mechanisms.

Workspace structure

- **_root folder_**
    - **\_LittleThumb\_**  
        - **automation**  
            dataset_register  
            job_register  
            dependency_graph  
            semaphores
        - **config**  
            config files
        - **datasets**  
            dataset descriptions
        - **jobs**  
            job descriptions
        - **history**  
            jobs.txt  
            commands.sh
    - **RAWREADS**  
        user defined, contains sequencing data
    - **PROCESSED**  
        user defined, ...

#### **2. Datasets** ####

A dataset is a collection of data sharing the same storage location as well as
the same storage format, and describded by the same table of metadata.

For instance, datasets can represent any collection of fatsq or bam files
associated with sequencing experiments, or read counts over genomic intervals,
or bed files with coordinates of ChIP-enriched regions, or bigwig files of
genomic profiles.

#### **3. Jobs & productions (planned)** ####

Jobs represent elementary executable tasks on datasets.

Productions represent series of jobs that can execute without any external
requirement except the availability of initial parameters and/or datasets.

Productions can include sub-productions?
Could be done by including jobs consisting in runing a sub-production?

#### **4. Configurations** ####

LittleThumb allows 5 levels of configuration with inheritance of
authorized settings at each level.

Default settings are inherited in the folowing order:  
`package > host -> user -> workspace -> job`

Authorized settings overwrite default settings in the reverse order:  
`job -> workspace -> user -> host -> package`

### **B. Implementations** ###

#### **1. Descriptions** ####

A description (S4 class = `LT_Description`) implements an hybrid data structure
combining a list and a data frame, internally named `properties` and `data`
respectively.

**+ Representation**

| Attribute    | Description                   |
| ------------ | ----------------------------- |
| `id`         | unique identifier (automated) |
| `properties` | `list` with named elements    |
| `data`       | `DataFrame`                   |

**+ Interface**

| Function             | Description                                       |
| -------------------- | ------------------------------------------------- |
| `properties`         | top level names of the `properties` list          |
| `property`           | accessor for top level `properties`               |
| `datakeys`           | column names in `data`                            |
| `ndata`              | number of rows in the `data` data frame           |
| `merge_descriptions` | merge `LT_Description` objects into a single one  |
| `split_descriptions` | split a `LT_Description` object into several ones |
| `save_description`   | save an `LT_Description` object to text file      |
| `load_description`   | load an `LT_Description` object from text file    |

**+ Common property accessors**

| Function  | Description                         |
| --------- | ----------------------------------- |
| `lt_id`   | LittleThumb identifier (read only)  |
| `lt_name` | user defined name                   |
| `lt_path` | file system directory               |

#### **2. Workspaces** ####

**+ Interface**

| Function          | Command         | Description                            |
| ----------------- | --------------- | -------------------------------------- |
| `list_workspaces` | list-workspaces | list available workspaces              |
| `load_workspace`  |                 | load workspace (R)                     |
| `save_workspace`  |                 | save workspace (R)                     |
| `list_datasets`   | list-datasets   | list available datasets                |
| `load_dataset`    |                 | load dataset (R)                       |
| `save_dataset`    |                 | save dataset (R)                       |

**+ Representation**

| Attribute      | Description                                                 |
| -------------- | ----------------------------------------------------------- |
| `lt_id`        | unique identifier -> default label for files and R objects  |
| `name`         | user defined name -> default label if specified + valid     |
| `path`         | file system folder (root of the workspace)                  |
| `cfg`          | list of config options                                      |
| `dts_register` | datasets register                                           |
| `jbs_register` | jobs register                                               |

**+ Datasets register**

| Attribute       | Description                                                |
| --------------- | ---------------------------------------------------------- |
| `lt_id`         | unique identifier -> default label for files and R objects |
| `name`          | user defined name -> default label if specified + valid    |
| `primary`       | TRUE if not derived from pre-existing dataset              |
| `src_workspace` | source workspace identifier (NA if none)                   |
| `src_dataset`   | source dataset identifier (NA if none)                     |
| `src_job`       | source job identifier                                      |
| `path`          | location of metadata (relative to workspace)               |
| `file`          | metadata file name (relative to path)                      |
| `data_type`     | accepted file formats (fastq, bam, bed, etc.)              |
| `data_path`     | location of actual data (relative to workspace)            |

**+ Jobs register**

| Attribute      | Description                                                 |
| -------------- | ----------------------------------------------------------- |
| `lt_id`        | unique identifier -> default label for files and R objects  |
| `name`         | user defined name -> default label if specified + valid     |

#### **3. Datasets** ####

Dataset metatdata must be accessed and updated easily either by editing the
associated text file or by modifying the associated R object.

Use hybrid file structure with 2 sections.
Section 1: commented lines with JSON formatted data
Section 2: tab delimited table with header column names

I/O functions:
Use readLines, split sections 1 and 2 (comment char), parse JSON, parse table,
join into object

**+ Interface**

**+ Representation**

| Attribute      | Description                                                 |
| -------------- | ----------------------------------------------------------- |
| `lt_id`        | unique identifier -> default label for files and R objects  |
| `name`         | user defined name -> default label if specified + valid     |
| `source`       | workspace, dataset, job identifier                          |
| `description_file` | name of the dataset description file                    |

**+ Dynamic load/save operations**

#### **4. Jobs** ####

**+ Interface**

**+ Representation**

| Attribute       | Description                                                |
| --------------- | ---------------------------------------------------------- |
| `lt_id`         | unique identifier                                          |
| `production`    | host computer, user, working dir, execution mode (bash, R) |
| `configuration` | versions, system level settings and default parameters     |
| `action`        | command call, function name                                |
| `parameters`    |                                                            |
| `source`        | workspace, dataset, input folder                           |
| `destination`   | workspace, dataset, output folder                          |

#### **5. Configurations** ####

**+ Package settings**

| Option         | Default   | Description                                     |
| -------------- | --------- | ----------------------------------------------- |
| `template`     | template  | template                                        |

**+ Host settings**

| Option         | Default   | Description                                     |
| -------------- | --------- | ----------------------------------------------- |
| `template`     | template  | template                                        |

**+ User settings**

| Option         | Default   | Description                                     |
| -------------- | --------- | ----------------------------------------------- |
| `template`     | template  | template                                        |

**+ Workspace settings**

| Option         | Default   | Description                                     |
| -------------- | --------- | ----------------------------------------------- |
| `CFGDIR`       | inherited | config files path (relative to workspace)       |
| `DTSDIR`       | inherited | datasets metadata path (relative to workspace)  |
| `JBSDIR`       | inherited | jobs metadata path (relative to workspace)      |

**+ Job settings**

| Option         | Default   | Description                                     |
| -------------- | --------- | ----------------------------------------------- |
| `template`     | template  | template                                        |

### **C. Deployment** ###

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
