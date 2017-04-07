# =============================================================================.
# Source:
# https://www.ebi.ac.uk/ena/browse/read-download#archive_generated_fastq_files
# -----------------------------------------------------------------------------.
# Archived fastq files are organised by run accession number under vol1/fastq
# directory in ftp.sra.ebi.ac.uk:
#
#   ftp://ftp.sra.ebi.ac.uk/vol1/fastq/<dir1>[/<dir2>]/<run accession>
#
#   <dir1> is the first 6 letters and numbers of the run accession
#          (e.g. ERR000 for ERR000916)
#
#   <dir2> does not exist if the run accession has six digits.
#          For example, fastq files for run ERR000916 are in directory:
#          ftp://ftp.sra.ebi.ac.uk/vol1/fastq/ERR000/ERR000916/.
#
# If the run accession has seven digits then the <dir2> is 00 + the last digit
# of the run accession. For example, fastq files for run SRR1016916 are in
# directory: ftp://ftp.sra.ebi.ac.uk/vol1/fastq/SRR101/006/SRR1016916/.
#
# If the run accession has eight digits then the <dir2> is 0 + the last two
# digits of the run accession.
#
# If the run accession has nine digits then the <dir2> is the last three digits
# of the run accession.
# -----------------------------------------------------------------------------.
ebi_ena_url <- function(sra) {
  nbr <- str_length(gsub("^[a-z]+", "", sra$run, perl = T, ignore.case = T))
  nbr <- unique(nbr)

  k <- nchar(sra$run)

  dir1 <- paste0(substr(sra$run, 1, 6), "/")
  dir2 <- ""
  if(nbr == 7) {
    dir2 <- paste0("00", substr(sra$run, k, k), "/")
  }
  if(nbr == 8) {
    dir2 <- paste0("0", substr(sra$run, k - 1, k), "/")
  }
  if(nbr == 9) {
    dir2 <- paste0(substr(sra$run, k - 2, k), "/")
  }
  paste0(
    "ftp://ftp.sra.ebi.ac.uk/vol1/fastq/", dir1, dir2, sra$run, "/", sra$run,
    ".fastq.gz"
  )
}
