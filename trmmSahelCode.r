require(plyr)
#path='~/trmm' 
path='/nobackupp4/datapool/trmm'

##---------------------------------------------------------------------
## FILE PRELIMS
## built in commands for getting files
theFiles <- list.files(path=path, pattern=glob2rx('*.nc'), full.names=TRUE)
head(theFiles)
## take the dates from the file names!
#str(strsplit(theFiles,'\\.'))
dates <- ldply( strsplit(theFiles,'\\.'), '[', c(2,3,4) )
## make the dates numeric instead of string
dates <- as.data.frame(llply(dates,as.numeric))
## name the columns
names(dates) <- c('year','month','day')
## I want to check I have a complete record... 
ddply( dates, .(year,month), summarize, dpm=length(as.numeric(day)))
## the following is the same task, with a more intuitive output.
require(reshape)
cast(dates, year~ month, value='day', fun.agg=length)
## judging by this, i have all the data.

##---------------------------------------------------------------------
## TIME
## deal with time using POSIX
## the paste command used in 2 ways.
mkDateStr <- function(r) paste(paste(r,collapse='/'),'12/00/00',sep='/')
## aaply dosent handle POSIX very well, so it's applied afterwards, its a
## vector operation anyways.
timeTrmm <-as.POSIXct(aaply( dates, 1, mkDateStr, .expand=FALSE ), format='%Y/%m/%d/%H/%M/%s', tz='UTC')

##---------------------------------------------------------------------
## SPACE
require(ncdf)
## read in a single file and get the coordinates common to all the gridded files.
ncdf.file <- open.ncdf(theFiles[1])
lat <- get.var.ncdf( ncdf.file, 'latitude' )
lon <- get.var.ncdf( ncdf.file, 'longitude' )
lon[which(lon>180)] <- lon[which(lon>180)] -360  ## east-west conversion
close.ncdf(ncdf.file)
nlat=length(lat); nlon=length(lon)
## grid out lat and lon to match the data dims.
lat.trmm <- as.vector(t(matrix( rep(lat,each=nlon), ncol=nlon, nrow=nlat, byrow=T)))
lon.trmm <- as.vector(t(matrix( rep(lon,each=nlat), ncol=nlon, nrow=nlat, byrow=F)))
## define the "sahel"
whSahel <- which( lat.trmm <= 35 & lat.trmm >= 0 & lon.trmm <= 15 & lon.trmm >=-15 )

## some spatial plots to check.
require(maps)
worldDf <- data.frame(map("world", plot=FALSE)[c("x","y")])
worldMap <-  geom_path(data=worldDf, aes(x=x, y=y))
sahelData=data.frame(lat=lat.trmm, lon=lon.trmm)[whSahel,]
ggplot() + worldMap + geom_tile( data=sahelData, aes(x=lon,y=lat,fill=lat) ) 

##---------------------------------------------------------------------
## READ IN THE DATA
## power it out
get.trmm.daily <- function(file) {
  print(file)
  ncdfFile <- open.ncdf(file)
  data <- get.var.ncdf( ncdfFile, 'r')
  close(ncdfFile)
  data[whSahel]  ## keep only what we want.
}
options(warn=1) ## doMC throws a warning and I dont want an error
require(doMC)
options(warn=2) ## go back to protective mode
registerDoMC(15) ## register 15 cores on the backend.
## results ntime obs of nspace locations/variables
system.time(trmmSahel <- laply( theFiles, get.trmm.daily, .parallel=TRUE ))

##---------------------------------------------------------------------
## VISUALIZE some things.

##-----------
## spatially plot time average values.
sahelTimeMean <- data.frame(mean=colMeans(trmmSahel), sahelData[c('lon','lat')])
options(warn=1) ## ggplot warns when it clips via xlim and ylim
ggplot() +
  worldMap +
  geom_tile( data=sahelTimeMean, aes(x=lon,y=lat,fill=mean) ) +
  xlim(-20, 20) + ylim(-5,40)
## change layer order so the map is over the data.
ggplot() +
  geom_tile( data=sahelTimeMean, aes(x=lon,y=lat,fill=mean) ) +
  worldMap +
  xlim(-20, 20) + ylim(-5,40)

##-----------
## let's now look at monthly spatial means.
sahelSpaceMean <- data.frame(mean=rowMeans(trmmSahel), month=format(timeTrmm, '%B'))
## just to order naturally instead of alphabetically
sahelSpaceMean$month <- factor(sahelSpaceMean$month, levels=unique(sahelSpaceMean$month))
sahelSpaceMean$year <- format(timeTrmm, '%Y')
## melting is a key skill for ggplot and other plyr tasks.
## sahelSpaceMeanMelt <- melt(sahelSpaceMean,id.vars=c('year','month'))
## this is like the aggregate command in base. very nice when data get complicated.
sahelSpMonMean <- ddply(sahelSpaceMean, .(year,month), summarize, monMean=mean(mean) )

## the show command is invoked by wrapping in ()
(hist <- ggplot(sahelSpMonMean) + geom_histogram(aes(x=monMean)))
## faceting is amazing, really exposes data in new ways.
hist +facet_wrap(~month)

## timeseries plot
ggplot(sahelSpMonMean, aes(x=year,y=monMean,group=month)) +
  geom_line() + geom_point() + facet_wrap(~month, nrow=1)

## somewhat nicer bar plot
ggplot(sahelSpMonMean, aes(x=year,y=monMean,group=month)) +
  geom_bar(fill='grey70',stat='identity',width=.00001) +
  geom_point() + facet_wrap(~month, nrow=1) + theme_bw()


