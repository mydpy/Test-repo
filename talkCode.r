##---------------------------------------------------------------------
## first class functions examples
data <- rnorm(1000) ## gaussian data

## non-example! more an excuse to show some R basics.
## not really functions in the list, rather their evaluations
mySummary <- function(x) { 
  list(mean=mean(x), median=median(x), sd=sd(x), mad=mad(x), IQR=IQR(x))
}
dataSummary <- mySummary(data) 
dataSummary
show(dataSummary)
print(dataSummary)
str(dataSummary)
dataSummary$median
dataSummary['median']
dataSummary[['median']]
dataSummary[2]
dataSummary[[2]]

##-----------
## let’s put functions in the list
summary2 <- function(x, statList) {
  for(iStat in 1:length(statList)) { statList[iStat]<-statList[[iStat]](x) }
  statList
}
funcList <- list(mean=mean, min=min, max=max)
str(funcList)
dataSummary2 <- summary2(data, funcList)
str(dataSummary2)

## let’s not use for on a list.
## also, note an anonymous function. and lapply accepts functions.
summary2.1 <- function(x, statList) lapply(statList, function(iStat) iStat(x))
str(summary2.1(data, list(mean=mean, min=min, max=max)))
## maybe silly to write that function.
str( lapply(list(mean=mean, min=min, max=max), function(iStat) iStat(data)) )

##-----------
## closure: a function which returns a function (with its environment).
## creates a new function which has fixed arguments to the orig func.
makeSummary <- function(funList)
  function(x) lapply(funList, function(iFun) iFun(x))
summary3 <- makeSummary(list(mean=mean, min=min, max=max))
str(summary3)
summary3
dataSummary3 <- summary3(data)
dataSummary3
makeSummary(c(mean, min, max))(data)

##---------------------------------------------------------------------
## make your own closure example
## change of log base
logBaseB <- function(b) function(x) log(x)/log(b)
log17 <- logBaseB(17)
str(log17)
17^log17(3)
17^logBaseB(17)(3)
pi^logBaseB(pi)(3)

##---------------------------------------------------------------------
## scoping / environments

## a not defined in the function env,
## looks for a in its enclosing env WHERE DEFINED.
f <- function() a/2  
a=10
f()

## where is f enclosed? where is it called?
g <- function() { a=100; f() }
g() 

## f is now defined in h but not modified in the global env.
h <- function() { a=100; f <- function() a/5; f() }
h()
## note neither a nor f is modified in the enclosing env.
a; f

## "rescope" f to g2. some frown on this but can be useful.
g2 <- function() { a=100; environment(f) <- environment(); f() }
g2()
## copy on modify! f was not harmed as orig defined.
f()

##-----------
## copy on modify: there are no pointers.
## if not modified, not copied.
k <- function(x) x <- x/2
## note the return does not involve a print, just assignment.
a=10
print(k(a)) ## why print?
a ## a is not changed

## likewise for variables not passed.
k2 <- function() a <- a/2  
print(k2())
a

##-----------
## <<- is lexical assignment. often frowned upon by gurus. 
if (exists('qq')) rm(qq)  ## erase qq variable.
## qq is set in the environment of h.  
h <- function(x) qq <- x
h(1)
exists('qq')

## lexical assignment searches for qq in enclosing environs.
## if not found before or global, it's created in the global env.
h2 <- function(x) qq <<- x
h2(1)
exists('qq'); rm(qq)

## remember, h2 is enclosed in the global env.
hLex <- function(x) {qq=3; h2(x); print(qq)}
hLex(1)
qq; rm(qq)

## nothing gets set in the global env
## because qq was found within hLex2
hLex2 <- function(x) {qq=3; environment(h2) <- environment(); h2(x); print(qq)}
hLex2(1)
qq

##---------------------------------------------------------------------
## functions can only return one object... lists! can contain anything
theList <- list(a=factor(1:4,levels=1:5),b=letters[1:4],runif(4,0,5))
str(theList)
str(theDf <- data.frame(a=factor(1:4,levels=1:5),b=letters[1:4],runif(4,0,5)))
str(as.data.frame(theList)) ## annoyingly converts the 3rd element to factor.

theList2 <- list(a=1:4, b=pi, c=c('a','j','k'), f=lm)
as.data.frame(theList2)

##-----------
## lists and data frames: call it by name
theDf ## dataframes have names=colnames and rownames
str(theDf) ## nrow obs of ncol vars
names(theDf)
colnames(theDf)
names(theDf)[3] <- 'unif'
rownames(theDf)
rownames(theDf) <- c("jan",'feb','mar','apr')
theDf
theDf$month <- as.factor(rownames(theDf)) ## indexing shown in str()
theDf
## blank selects the entire dimension (row)
theDf[,c('month','b')] ##  single [ returns a sublist
theDf[[,'month']] ## [[ returns the object in the sublist, can only select 1 column
theDf[-3,] ## can exclude indices with neg index numbers (cant negate names)
theDf[-1*(which(rownames(theDf)=='mar')),] ## which returns index on logical
theDf[!(rownames(theDf)=='mar'),] ## can also use logical indices


##---------------------------------------------------------------------
## OO in R: S3 & S4 classes
df <- data.frame(x1=1:100, x2=abs(-50:49))
df$y <- 4*df$x1 + -5*df$x2 + rnorm(100)
str(theFit <- lm(y~., df)) ## S3 object
names(theFit)
class(theFit)  ## this is how it's different from a list.
predict       ## a general method
predict.lm    ## the above calls this after checking the class
predict(theFit, data.frame(x1=75,x2=-4))
predict.lm(theFit, data.frame(x1=75,x2=-4))

##---------------------------------------------------------------------
## plyr
?llply
theList <- list(a=1:4, c=c('a','j','k'),
                f=rnorm(100), l=list(x=1:4,y=rnorm(4)))
str(ele2 <- llply(theList, '[[', 2)) ## subsetting functions '[','[['
str(list2 <- llply(theList, '[', 2)) ## the list data is different

##---------------------------------------------------------------------
## Riemann sums example
theFunc <- function(x) 4/(1+x^2) ## x is a vector
riemannMid <- function(n,f) sum( f(seq(0+(1/n)/2,1-(1/n)/2,1/n)) *1/n )
riemannMidClosure <- function(f)
  function(n) sum( f(seq(0+(1/n)/2,1-(1/n)/2,1/n)) *1/n)  
system.time( print(riemannMid(1e7,theFunc)) )['elapsed']
system.time( print(riemannMidClosure(theFunc)(1e7)) )[3]

## let's look at the accuracy as a function of n.
laply( 10^(1:6), riemannMid, f=theFunc)-pi
## look at time as a function of n.
laply( 10^(1:7), function(n) system.time(riemannMid(n,theFunc)-pi)[3] )
## do it in a nicer way, actually use a named list for input.
inN <- as.list(10^(1:7)); names(inN) <- unlist(inN)
laply( inN, function(n) system.time(riemannMid(n,theFunc)-pi)[3] )
llply( inN, function(n) system.time(riemannMid(n,theFunc)-pi)[3] )
## clearly, this is a much nicer format for plotting than any of the above.
print(timeDf <- ldply( inN, function(n) system.time(riemannMid(n,theFunc)-pi)[3] ))
plot(timeDf$.id, timeDf$elapsed,log='x') ## note coercion of x to numeric
ggplot( timeDf, aes(x=.id,y=elapsed)) + geom_point()
ggplot( timeDf, aes(x=as.numeric(.id),y=elapsed)) + geom_point() +
  geom_line() +scale_x_log10()
## what about precision v time?

##
## demonstrate plyr backend
## on bridge4
## cat /proc/cpuinfo
## 64 procs with 8 cores each
install.packages("plyr")
install.packages("doMC")
require(plyr) ## require() vs library()
require(doMc)
registerDoMC(10) ## 10 cores, so we are using 2 processors.
getDoParWorkers()
dum<-function(z) { x=0; while(x<1e7) x=x+1; 1 }
llply( 1:getDoParWorkers(), dum, .parallel=TRUE )

##
