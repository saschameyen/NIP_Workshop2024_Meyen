################################################################################
#' 
#' Source all R scripts in the project.
#' 
#' Dr. Sascha Meyen, saschameyen@gmail.com
#' 2023-02-08 17:47
#' 
################################################################################

verbose <- TRUE

# Find all .R files in subdirectories
R_files <- list.files(path         = c('Scripts'), 
                      pattern      = '*.R$'      , 
                      full.names   = TRUE        , 
                      recursive    = TRUE        , 
                      include.dirs = FALSE       )

# Source them
if (verbose) cat('\nSourcing all analysis scripts.\n')
for (i in 1:length(R_files))
{
  if (verbose)
    cat(sprintf('Sourcing %s/%s: %s\n', 
                formatC(i, width = 2, flag = '0'), 
                formatC(length(R_files), width = 2, flag = '0'),
                R_files[i]))

  invisible(sapply(R_files[i], source)) 
}

if (verbose) cat('\n\n')
