## ----message=FALSE, include=FALSE----------------------------------------
library(LittleThumb)

# Reset
rm(list = objects())
unlink("AutoSaved", recursive = T)

cfg <- LittleThumb()

## ----mini_script, eval=FALSE---------------------------------------------
#  # MiniScript.R
#  
#  library(LittleThumb)
#  
#  # 1. Configure global options --------------------------------------------------
#  
#  # Here we choose the default location for automatically saved RDS files
#  LittleThumb(rootpath = "AutoSaved")
#  
#  # 2. Define persistent R objects -----------------------------------------------
#  
#  MakeObj(xyz, {
#  
#    # Here we compute the value of the object
#    xyz <- 0
#  
#  })
#  
#  # 3. Do anything with defined R objects ----------------------------------------
#  
#  print(xyz)

## ----eval=FALSE----------------------------------------------------------
#  source("MiniScript.R") # First execution

## ----eval=TRUE, message=TRUE, echo=FALSE, results='hide', ref.label='mini_script'----
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

## ----eval=FALSE----------------------------------------------------------
#  source("MiniScript.R") # Second execution

## ----eval=TRUE, message=TRUE, echo=FALSE, results='hide', ref.label='mini_script'----
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

## ------------------------------------------------------------------------
rm(xyz)

## ----eval=FALSE----------------------------------------------------------
#  source("MiniScript.R") # Another execution after loss of 'xyz'

## ----eval=TRUE, message=TRUE, echo=FALSE, results='hide', ref.label='mini_script'----
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

## ------------------------------------------------------------------------
LittleThumb(reload = "xyz")

## ----eval=FALSE----------------------------------------------------------
#  source("MiniScript.R") # Another execution after loss of 'xyz'

## ----eval=TRUE, message=TRUE, echo=FALSE, results='hide', ref.label='mini_script'----
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

## ------------------------------------------------------------------------
LittleThumb(rebuild = "xyz")

## ----eval=FALSE----------------------------------------------------------
#  source("MiniScript.R") # Another execution after loss of 'xyz'

## ----eval=TRUE, message=TRUE, echo=FALSE, results='hide', ref.label='mini_script'----
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

## ------------------------------------------------------------------------
# Naive DNA sequence simulation
SimSeq <- function(n, l, gc = 0.4) {

  a <- factor(c("A", "C", "G", "T"))
  p <- c(1 - gc, gc, gc, 1 - gc) / 2
  
  s <- replicate(n, sample(a, size = l, replace = T, prob = p), simplify = F)
  names(s) <- paste0("S", 1:n)
  
  s
}

## ------------------------------------------------------------------------
# Chaos Game Representation of DNA
CGR <- function(s) {

  s <- as.numeric(s)

  x <- c(-1, -1,  1,  1)[s]
  y <- c(-1,  1,  1, -1)[s]
  
  for(i in 2:length(s)) {
    x[i] <- (x[i - 1] + x[i]) / 2
    y[i] <- (y[i - 1] + y[i]) / 2
  }
  
  cbind(x, y)
}

## ------------------------------------------------------------------------
# Here we choose the default location for automatically saved RDS files
LittleThumb(rootpath = "AutoSaved")

# We use specific paths for contained objects
cache <- list(
  dna    = "contents/dna",
  graphs = "contents/graphs"
)

## ----make_dna, eval=FALSE------------------------------------------------
#  MakeObj(dna, {
#  
#    # Initialize dna as a list specifying the length and number of DNA sequences
#    dna <- list(s_len = 10000, s_nbr = 4)
#  
#    MakeObj(sequences, path = cache$dna, {
#      sequences <- with(dna, SimSeq(s_nbr, s_len))
#    })
#  
#    AssignObj(sequences, to = dna)
#  })

## ----make_graphs, eval=FALSE---------------------------------------------
#  MakeObj(graphs, {
#  
#    # Initialize graphs as an empty environment
#    graphs <- new.env()
#  
#    for(i in 1:dna$s_nbr) {
#  
#      obj <- names(dna$sequences[i])
#  
#      MakeObj(name = obj, path = cache$graphs, envir = graphs, rebuild = T, {
#        m <- CGR(dna$sequences[[i]])
#        AssignObj(m, name = obj)
#      })
#    }
#  })

## ----eval=TRUE, message=TRUE, echo=FALSE, results='hide', ref.label='make_dna'----
MakeObj(dna, {
  
  # Initialize dna as a list specifying the length and number of DNA sequences
  dna <- list(s_len = 10000, s_nbr = 4) 

  MakeObj(sequences, path = cache$dna, {
    sequences <- with(dna, SimSeq(s_nbr, s_len))
  })
  
  AssignObj(sequences, to = dna)
})

## ----eval=TRUE, message=TRUE, echo=FALSE, results='hide', ref.label='make_graphs'----
MakeObj(graphs, {

  # Initialize graphs as an empty environment
  graphs <- new.env()

  for(i in 1:dna$s_nbr) {
    
    obj <- names(dna$sequences[i])
    
    MakeObj(name = obj, path = cache$graphs, envir = graphs, rebuild = T, {
      m <- CGR(dna$sequences[[i]])
      AssignObj(m, name = obj)
    })
  }
})

## ----script, eval=FALSE--------------------------------------------------
#  library(LittleThumb)
#  
#  LittleThumb(rootpath = "AutoSaved")
#  
#  MakeObj(x, {
#    message("building object x...")
#    x <- pi
#  })

## ----eval=FALSE----------------------------------------------------------
#  source("LT_Test.R")

## ----eval=TRUE, message=TRUE, echo=FALSE, results='hide', ref.label='script'----
library(LittleThumb)

LittleThumb(rootpath = "AutoSaved")

MakeObj(x, {
  message("building object x...")
  x <- pi
})

## ----eval=FALSE----------------------------------------------------------
#  source("LT_Test.R")

## ----eval=TRUE, message=TRUE, echo=FALSE, results='hide', ref.label='script'----
library(LittleThumb)

LittleThumb(rootpath = "AutoSaved")

MakeObj(x, {
  message("building object x...")
  x <- pi
})

## ----include=FALSE-------------------------------------------------------
# Simulate restarting the R environment
do.call(LittleThumb, cfg) # Restore default options
suppressWarnings(rm(x))   # Cleanup

## ----eval=FALSE----------------------------------------------------------
#  source("LT_Test.R")

## ----eval=TRUE, message=TRUE, echo=FALSE, results='hide', ref.label='script'----
library(LittleThumb)

LittleThumb(rootpath = "AutoSaved")

MakeObj(x, {
  message("building object x...")
  x <- pi
})

## ----include=FALSE-------------------------------------------------------
x <- 0

## ----eval=FALSE----------------------------------------------------------
#  x <- 0
#  source("LT_Test.R")

## ----eval=TRUE, message=FALSE, echo=FALSE, results='hide', ref.label='script'----
library(LittleThumb)

LittleThumb(rootpath = "AutoSaved")

MakeObj(x, {
  message("building object x...")
  x <- pi
})

## ------------------------------------------------------------------------
print(x)

## ----script_reload, eval=FALSE-------------------------------------------
#  library(LittleThumb)
#  
#  LittleThumb(rootpath = "AutoSaved")
#  LittleThumb(reload = T)
#  
#  MakeObj(x, {
#    message("building object x...")
#    x <- pi
#  })

## ----eval=FALSE----------------------------------------------------------
#  library(LittleThumb)
#  
#  LittleThumb(rootpath = "AutoSaved")
#  
#  MakeObj(x, reload = T, {
#    message("building object x...")
#    x <- pi
#  })

## ----eval=FALSE----------------------------------------------------------
#  source("LT_Test.R")

## ----eval=TRUE, message=TRUE, echo=FALSE, results='hide', ref.label='script_reload'----
library(LittleThumb)

LittleThumb(rootpath = "AutoSaved")
LittleThumb(reload = T)

MakeObj(x, {
  message("building object x...")
  x <- pi
})

## ------------------------------------------------------------------------
print(x)

## ----include=FALSE-------------------------------------------------------
do.call(LittleThumb, cfg) # Restore default options

## ------------------------------------------------------------------------
MakeObj(x, { x <- 2 * pi })

## ------------------------------------------------------------------------
print(x)

## ----script_rebuild, eval=FALSE------------------------------------------
#  library(LittleThumb)
#  
#  LittleThumb(rootpath = "AutoSaved")
#  LittleThumb(rebuild = T)
#  
#  MakeObj(x, {
#    message("building object x...")
#    x <- 2 * pi
#  })

## ----eval=FALSE----------------------------------------------------------
#  library(LittleThumb)
#  
#  LittleThumb(rootpath = "AutoSaved")
#  
#  MakeObj(x, rebuild = T, {
#    message("building object x...")
#    x <- 2 * pi
#  })

## ----eval=FALSE----------------------------------------------------------
#  source("LT_Test.R")

## ----eval=TRUE, message=TRUE, echo=FALSE, results='hide', ref.label='script_rebuild'----
library(LittleThumb)

LittleThumb(rootpath = "AutoSaved")
LittleThumb(rebuild = T)

MakeObj(x, {
  message("building object x...")
  x <- 2 * pi
})

## ------------------------------------------------------------------------
print(x)

## ----include=FALSE-------------------------------------------------------
do.call(LittleThumb, cfg) # Restore default options

## ----include=FALSE-------------------------------------------------------
# Cleanup
unlink("AutoSaved", recursive = T)
LittleThumb::ResetOptions()
DeleteObj(x)

