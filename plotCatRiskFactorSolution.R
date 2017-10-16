#!/usr/bin/env Rscript

#### This script takes as input a catagorical risk factor solution.
# 


#options( warn=0, error=recover)
#options( warn=2, error=recover)
#options( warn=0, error=NULL)
options( warn=2, error=NULL) # use this setting for formal testing

#print( sessionInfo())
#fnStr(options("warn"))
#fnStr(options("error"))
#fnStr(getwd())



options(stringsAsFactors = FALSE) # hurray!!!
#class( data.frame(letters)[,1])

# reload library
sPkg <- "diseaseRiskPredictor"
suppressPackageStartupMessages( library(sPkg,char=T) )



fnCalcArea <- function( vX, vY, xLo=-Inf, xHi=Inf ){
	vix <- which(vX>=xLo & vX<=xHi)
	seqArea <- 0
	for( ix in min(vix):(max(vix)-1) ) seqArea <- seqArea + (vX[ix+1]-vX[ix])*mean(vY[ix:(ix+1)])
	seqArea
}



#### MMMM ##### program starts here ##############################################

if( interactive() ){
	sWD <- "C:/Desmond/diseaseRiskPredictor/testHarness"
	setwd(sWD)
}

sFileSol <- "CategoricalRiskFactorTestHarness.out.tsv"

fnRep( paste("Read categorical risk factor solution from file -", sFileSol ))
# read data from file
dfSol <- read.table( sFileSol, header=T, sep="\t", stringsAsFactors=F)
fnRep( "Categorical Risk Factor solution is" )
dfSol



# extra required parameters from solution
vFreq <- dfSol$freq
vMean <- dfSol$liaMean

liaVar <- dfSol$liaVarIntra
if( max(liaVar)-min(liaVar) > 1e-3 ) stop( "High spread for liaVar")
liaVar <- mean(liaVar)

liaThr <- dfSol$liaThr_imp
if( max(liaThr)-min(liaThr) > 1e-3 ) stop( "High spread for liaThr")
liaThr <- mean(liaThr)
liaThr
liaThr/qnorm(1-0.001)

dfInput <- data.frame(freq=vFreq, mean=vMean, sd=sqrt(liaVar), bRef=dfSol$bRefStratum )
fnRep( "Risk Factor strata distributions")
dfInput



vX <- seq(-3,3, by=0.002)

# calc strata liability scaled pdf
mY <- NULL
for( ss in 1:nrow(dfInput)){
	vY <- dfInput$freq[ss] * dnorm( vX, mean=dfInput$mean[ss], sd=dfInput$sd[ss] )
	mY <- cbind(mY,vY)
}

# calc population liability pdf
vY <- apply( mY, 1, sum)
mY <- cbind(mY,vY)
colnames(mY)<- c(paste0("strata",1:nrow(dfInput)),"pop")
str(mY)



# plot risk factor strata liability distribution
par(mfrow=c(2,1))
vYlim <- range( c(0,range(mY)))
vsColour <- rainbow(nrow(dfInput))
sMain <- "Risk Strata Liability Probability Densities scaled by Strata Frequencies"
plot( x=0, y=0, type="n", xlim=range(vX), ylim=vYlim, main=sMain, xlab="liability", ylab="scaled density" )
abline( h=0,col="grey")
for( cc in 1:(ncol(mY)-1) ){
vY <- mY[,cc]
lines( x=vX, y=vY, col=vsColour[cc] )
abline( v=dfInput$mean[cc], lty="dashed", col=vsColour[cc] )
}
abline( v=liaThr, col="grey" )

# plot population liability distribution
sMain <- "Population Liability Probability Density"
plot( x=0, y=0, type="n", xlim=range(vX), ylim=vYlim, main=sMain, xlab="liability", ylab="density" )
abline( h=0,col="grey")
lines( x=vX, y=mY[,ncol(mY)] )
abline( v=0, lty="dashed")
abline( v=liaThr, col="grey" )



# check freq
fnRep("The area under the curve (AUC) should equal the strata frequency")
vAUC <- apply( mY, 2, function( vY) fnCalcArea( vX, vY ) )
vAUC/c(dfInput$freq,1)

# check mean
fnRep("For symmetric distributions, i.e. the strata distributions, the AUCs pre and post mean should be equal")
vAUCPreMean <- NULL
for(cc in 1:ncol(mY)) vAUCPreMean <- c( vAUCPreMean, fnCalcArea( vX, mY[,cc], xHi=c(dfInput$mean,0)[cc] ) )
vAUCPostMean <- NULL
for(cc in 1:ncol(mY)) vAUCPostMean <- c( vAUCPostMean, fnCalcArea( vX, mY[,cc], xLo=c(dfInput$mean,0)[cc] ) )
vAUCPostMean/vAUCPreMean

# check risks
vAUCAboveThr <- apply( mY, 2, function( vY) fnCalcArea( vX, vY, xLo=liaThr ) )
fnRep("The risks per strata are ")
vRisk <- vAUCAboveThr/vAUC
vRisk

fnRep("The relative risks per strata are ")
riskBaselineRef <- vRisk[-length(vRisk)][dfInput$bRef]
vRisk/riskBaselineRef
(vRisk/riskBaselineRef)/c(dfSol$relRisk,NA)



fnRep("Completed successfully")
