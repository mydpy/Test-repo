## Tips for getting running in R.
## These are the essentials of what I use.
## James McCreight - mccreigh (at) g mail .dot. com
##---------------------------------------------------------------------

## in my .bashrc (or .bash_profile), I typically make the following alias
## to R because I dont use saved workspaces and this makes life smoother.
## R--help shows more options.
alias R='r --no-save --no-restore-data'

## the following 2 'dot' files go in your home dir
##-----------
## .Rprofile file
## sets the cran repository so it stops asking you. at is austria, the mother repo.
options(repos="http://cran.at.r-project.org")

##-----------
## .Renviron file
## setting my profile which gets run at startup
R_PROFILE=/Users/james/R/startup_jlm.r
## my default library location for packages installed w/in R.
## note this can be shared given the proper permissions.
## i think mine is shared if you'd like to use it on lnxsrv105.
R_LIBS=/nobackup/username/R/lib

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
