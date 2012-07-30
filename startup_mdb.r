##---------------------------------------------------------------------
## my R_PROFILE file (lives where and named as specified .Renviron).
## can call R --vanilla to skirt running this file.
options(warn=2)  ## throws an error at each warning
options(error=utils::recover)  ## gives debugging functionality
require(ggplot2) 
require(plyr)
options(warn=2)

## using src() instead of source() puts the unix PATH in your R path.
## this way you can build R libraries which you can includ in your
## normal path.
## not sure if this works on windows.... it may.
src <- function(file, path=Sys.getenv("PATH"), ...)
{
  path=paste(path,getwd(),sep=.Platform$path.sep)
  for (p in strsplit(path,.Platform$path.sep)[[1]])
    { ## this does it recursively
      files.path <- list.files(p, recursive=TRUE, full.names=TRUE)
      files <- unlist(lapply(strsplit(files.path,.Platform$file.sep),tail,1))
      wh.match=match(file, files)
      if(!is.na(wh.match)) return(source(files.path[wh.match], ...))
    }
  stop("file ", sQuote(file), " not found") 
}

## a convenient way to get a blank workspace while refreshing my defaults.
.reset <- function() {
  rm(list=ls(envir=as.environment('.GlobalEnv')), envir=as.environment('.GlobalEnv'))
  source(as.character(Sys.getenv("R_PROFILE"))) 
}

