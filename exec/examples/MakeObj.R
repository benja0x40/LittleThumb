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

# Cleanup ======================================================================
DeleteObj(xyz)
unlink("AutoSaved", recursive = TRUE)
