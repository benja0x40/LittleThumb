## ----message=FALSE, include=FALSE----------------------------------------
library(LittleThumb)

cfg <- LittleThumb()

# DeleteObj(x)
# DeleteObj(a, path = "text")
# DeleteObj(v, path = "values")

unlink("./LT_Tests", recursive = T)

## ----eval=FALSE----------------------------------------------------------
#  library(LittleThumb)
#  
#  LittleThumb(path = "./LT_Tests")
#  
#  LittleThumb(
#    rebuild = list(
#      dna = T,
#      cgr = T
#    ),
#    overload = c(
#      "a", "s", # used in dna
#      "x", "y"  # used in cgr
#    )
#  )
#  
#  subdir <- list(
#    dna = "data_dna",
#    cgr = "data_obj2"
#  )
#  
#  seq_sim <- function(a, n, l) {
#    s <- replicate(n, sample(a, size = l, replace = T), simplify = F)
#    names(s) <- paste0("sequence_", 1:n)
#  }
#  
#  MakeObj(dna, {
#  
#    dna <- list(
#      alpha = factor(c("A", "C", "G", "T")),
#      s_len = 100, # Length of simulated sequences
#      s_nbr = 9    # Number of simulated sequences
#    )
#  
#    MakeObj(sequences, path = subdir$dna, {
#      s <- with(dna, seq_sim(alpha, s_nbr, s_len))
#    })
#  
#  })
#  
#  MakeObj(cgr, {
#  
#    cgr <- list()
#  
#    for(i in 1:dna$s_nbr) {
#      obj <- names(s)[i]
#      MakeObj(name = obj, path = subdir$cgr, {
#        s <- with(dna, seq_sim(alpha, s_nbr, s_len))
#      })
#    }
#  
#  
#  })
#  

## ----script, eval=FALSE--------------------------------------------------
#  library(LittleThumb)
#  
#  LittleThumb(path = "./LT_Tests")
#  
#  MakeObj(x, {
#    message("building object x...")
#    x <- pi
#  })

## ----eval=FALSE----------------------------------------------------------
#  source("LT_Test.R")

## ----eval=TRUE, message=TRUE, echo=FALSE, results='hide', ref.label='script'----
library(LittleThumb)

LittleThumb(path = "./LT_Tests")

MakeObj(x, {
  message("building object x...")
  x <- pi
})

## ----eval=FALSE----------------------------------------------------------
#  source("LT_Test.R")

## ----eval=TRUE, message=TRUE, echo=FALSE, results='hide', ref.label='script'----
library(LittleThumb)

LittleThumb(path = "./LT_Tests")

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

LittleThumb(path = "./LT_Tests")

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

LittleThumb(path = "./LT_Tests")

MakeObj(x, {
  message("building object x...")
  x <- pi
})

## ------------------------------------------------------------------------
print(x)

## ----script_reload, eval=FALSE-------------------------------------------
#  library(LittleThumb)
#  
#  LittleThumb(path = "./LT_Tests")
#  LittleThumb(overload = T)
#  
#  MakeObj(x, {
#    message("building object x...")
#    x <- pi
#  })

## ----eval=FALSE----------------------------------------------------------
#  library(LittleThumb)
#  
#  LittleThumb(path = "./LT_Tests")
#  
#  MakeObj(x, overload = T, {
#    message("building object x...")
#    x <- pi
#  })

## ----eval=FALSE----------------------------------------------------------
#  source("LT_Test.R")

## ----eval=TRUE, message=TRUE, echo=FALSE, results='hide', ref.label='script_reload'----
library(LittleThumb)

LittleThumb(path = "./LT_Tests")
LittleThumb(overload = T)

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
#  LittleThumb(path = "./LT_Tests")
#  LittleThumb(rebuild = T)
#  
#  MakeObj(x, {
#    message("building object x...")
#    x <- 2 * pi
#  })

## ----eval=FALSE----------------------------------------------------------
#  library(LittleThumb)
#  
#  LittleThumb(path = "./LT_Tests")
#  
#  MakeObj(x, rebuild = T, {
#    message("building object x...")
#    x <- 2 * pi
#  })

## ----eval=FALSE----------------------------------------------------------
#  source("LT_Test.R")

## ----eval=TRUE, message=TRUE, echo=FALSE, results='hide', ref.label='script_rebuild'----
library(LittleThumb)

LittleThumb(path = "./LT_Tests")
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
# Simulate restarting the R environment
do.call(LittleThumb, cfg) # Restore default options
suppressWarnings(rm(x))   # Cleanup

## ----script_multi, eval=FALSE--------------------------------------------
#  library(LittleThumb)
#  
#  LittleThumb(path = "./LT_Tests", overload = T)
#  
#  MakeObj(x, { x <- 2 * pi })
#  
#  MakeObj(data, rebuild = T, {
#  
#    data <- new.env(parent = globalenv())
#  
#    MakeObj(k, path = "text", envir = data, {
#      k <- LETTERS[1:5]
#    })
#  
#    MakeObj(v, path = "values", envir = data, {
#      v <- 1:5
#    })
#  
#  })

## ----eval=FALSE----------------------------------------------------------
#  source("LT_Test.R")

## ----eval=TRUE, message=TRUE, echo=FALSE, results='hide', ref.label='script_multi'----
library(LittleThumb)

LittleThumb(path = "./LT_Tests", overload = T)

MakeObj(x, { x <- 2 * pi })

MakeObj(data, rebuild = T, {
  
  data <- new.env(parent = globalenv())
  
  MakeObj(k, path = "text", envir = data, {
    k <- LETTERS[1:5]
  })
  
  MakeObj(v, path = "values", envir = data, {
    v <- 1:5
  })

})

## ------------------------------------------------------------------------
print(data$k)
print(data$v)

## ----include=FALSE-------------------------------------------------------
# Cleanup
DeleteObj(x)
unlink("./LT_Tests", recursive = T)

