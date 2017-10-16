#!/usr/bin/env Rscript



#options( warn=0, error=recover)
#options( warn=2, error=recover)
options( warn=0, error=NULL)
#options( warn=2, error=NULL) # use this setting for formal testing

#print( sessionInfo())
#fnStr(options("warn"))
#fnStr(options("error"))
#fnStr(getwd())



options(stringsAsFactors = FALSE) # hurray!!!

# reload library
sPkg <- "diseaseRiskPredictor"
suppressPackageStartupMessages( library(sPkg,char=T) )
if(T){ # overwrite kinship2::plot.pedigree()
  fnRep( "patch kinship2::plot.pedigree()")
  assignInNamespace( "plot.pedigree", diseaseRiskPredictor:::plot.pedigree.FIXED, ns="kinship2")
}



#' Calculates panel plot dimensions
fnCalcPanelPlotDim <- function( nofPlots, maxNofCol=4)
{
  vDim <- rep(floor(sqrt(nofPlots)),2)
  while(T){
    if(vDim[1]*vDim[2]>=nofPlots) break
    if(vDim[2]==vDim[1]) vDim[2] <- vDim[2]+1 else vDim[1] <- vDim[1]+1
    if( vDim[2]==maxNofCol ) break
  }
  vDim 
}



#### MMMM ##### program starts here ##############################################

bCmdLine <- !interactive()
if( bCmdLine) {
  vsArgs <- commandArgs(TRUE)
  options(width=200)
  
} else {
  options(width=200)
  sWD <- "C:/Desmond/diseaseRiskPredictor/testHarness"
  setwd(sWD)
  
  # specify command
  sCmdArgs <- "-d ./validateNYearRisk/dm.schizophrenia.txt -p ./validateNYearRisk/testPedDes.tsv"
  sCmdArgs <- "--diseaseModel=diseaseModels/dm.Con1.txt -p pedigrees/ped.p2c3.affectedMz.tsv -s  -i 20"
  sCmdArgs <- "-d ./validateNYearRisk/dm.schizophrenia.txt -p ./validateNYearRisk/testPedDes.tsv -y 10"
  sCmdArgs <- "-d diseaseModels/dm.schizophrenia.txt -p ./pedigrees/ped.tmp.tsv"
  sCmdArgs <- "-d diseaseModels/dm.Con1.txt -p pedigrees/ped.p2c3.deceasedNA.tsv"

  vsArgs <- strsplit(split=" +",sCmdArgs)[[1]]
}	
fnRep("Inputs are")
fnStr(vsArgs)

suppressPackageStartupMessages(library('optparse'))
option_list <- list( 
  make_option(c("-d", "--diseaseModel"),  action="store",      help="path to disease model file",                                                         type="character" ),
  make_option(c("-p", "--pedigree"),      action="store",      help="path to pedigree information file",                                                  type="character" ),
  make_option(c("-s", "--stdDev"),        action="store_true", help="calculate standard deviation of risk estimates [default %default]",                  default=FALSE ),
  make_option(c("-i", "--nofIterations"), action="store",      help="number of iteratations to use for standard deviation estimation [default %default]", default=10, type="integer" ),
  make_option(c("-y", "--nofYears"),      action="store",      help="estimate risk of becoming affected within the next n years [default %default]",      default=5,  type="integer" ),
  make_option(c("-n", "--nofDraws"),      action="store",      help="number of draws from pedigree posterior liability distribution [default %default]",  default=20000,  type="integer" ),
  make_option(c("-b", "--nofBurnIn"),     action="store",      help="number of draws for Gibbs Sampler burn in [default %default]",                       default=1000,  type="integer" )
)

# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults
opt <- tryCatch( parse_args( OptionParser(option_list=option_list), args=vsArgs )
                 , warning=function(w) w$message, error=function(e) e$message
) # end of tryCatch
# report error if command line parsing failed
if( is.character(opt) ) stop( "Problem with command line arguments. ", opt )

# further validation of command line args

# validate required
if( is.null(opt$diseaseModel) ) stop( "Problem with command line argument: diseaseModel. ", "This required argument is missing from the command line")
if( is.null(opt$pedigree) )     stop( "Problem with command line argument: pedigree. ", "This required argument is missing from the command line")

# validate value
if( is.na(opt$nofIterations) )  stop( "Problem with command line argument: nofIterations. ", "Value should be an integer" )
if( opt$nofIterations<1 )       stop( "Problem with command line argument: nofIterations. ", "Value should be a positive integer" )

if( is.na(opt$nofYears) )  stop( "Problem with command line argument: nofYears. ", "Value should be an integer" )
if( opt$nofYears<0 )       stop( "Problem with command line argument: nofYears. ", "Value should be a positive integer (or 0 for current risk)" )

if( is.na(opt$nofDraws) )  stop( "Problem with command line argument: nofDraws. ", "Value should be an integer" )
if( opt$nofDraws<0 )       stop( "Problem with command line argument: nofDraws. ", "Value should be a positive integer" )

if( is.na(opt$nofBurnIn) )  stop( "Problem with command line argument: nofBurnIn. ", "Value should be an integer" )
if( opt$nofBurnIn<0 )       stop( "Problem with command line argument: nofBurnIn. ", "Value should be a positive integer" )

fnRep("Program inputs are")
fnStr(opt)



# read inputs into appropriate variables
sFileDis <- opt$diseaseModel
sFilePed <- opt$pedigree



fnRep("Below is some diagnostic information regarding the setup")
fnRep("sessionInfo() reports")
print( sessionInfo())
cat("\n")
fnRep("system('g++ -v') reports")
print(system('g++ -v'))
cat("\n")
fnRep("Sys.getenv('PATH') reports")
print(Sys.getenv('PATH'))
print(strsplit( split=";",Sys.getenv('PATH'))[[1]])
cat("\n")
if( 'devtools' %in% installed.packages()[,"Package"] ){
  fnRep("devtools::find_rtools(debug=TRUE) reports")
  print(devtools::find_rtools(debug=TRUE))
  cat("\n")
}



fnRep( paste("Read pedigree from file -", sFilePed ))
# read data from file
dfPed <- read.table( sFilePed, header=T, sep="\t", stringsAsFactors=F)
fnRep( "Pedigree is" )
dfPed

fnRep( paste("Read disease model from file -", sFileDis ))
lDis <- NA; lDis <- fnReadFileDis( sFileDis )
cat("\n")
fnRep( "Disease model is" )
lDis



# creates a file for any plots to go into
# rely on process death to close the plot file
bPlotToFile <- !interactive()
if(bPlotToFile) {
  sFilePlot <- "MultifactorialDiseaseRiskCalculator.R.plots.pdf"
  fnRep("Create file for any plots to be written into -", sFilePlot )
  fnStr(sFilePlot)
  pdf(sFilePlot, paper="a4r", width=10, height=7)
  iDevFilePlot <- dev.cur()
  iDevFilePlot
}

fnRep("validate pedigree")
dfPed <- fnValidatePedigree( dfPed )

fnRep("plot pedigree")
oPedigree <- NA; oPedigree <- fnConstructPedigree( dfPed )
oPlotPed <- plot(oPedigree)
title(sFilePed)

fnRep('calc pedigree prior liability distribution')
lPedDis <- fnPrepareForRiskPrediction( dfPed, lDis, bVerbose=T )
names(lPedDis)

llRes <- list()
# draw samples from pedigree posterior liability distribution
nofDraws <- opt$nofDraws
nofBurnIn <- opt$nofBurnIn
nofYears <- opt$nofYears
iNofRepeats <- ifelse( !opt$stdDev, 1, opt$nofIterations )
for( rr in 1:iNofRepeats ) {
  fnRep("iteration", rr, "of", iNofRepeats)
  
  fnRep('draw samples from pedigree posterior liability distribution')
  lRes <- fnPredictRisk( lPedDis, nofBurnIn=nofBurnIn, nofDraws=nofDraws, nofYears=nofYears, bVerbose=T )
  names(lRes)
  
  # assess burn-in
  fnRep("assess burn-in")
  mTotLiaSample <- lRes$mTotLiaSample
  par( mfrow=fnCalcPanelPlotDim(ncol(mTotLiaSample)) )
  sX <- 'iteration'
  sY <- 'Liability'
  for(pp in 1:ncol(mTotLiaSample)) {
    plot( mTotLiaSample[,pp], main=colnames(mTotLiaSample)[pp], xlab=sX, ylab=sY, pch="." ); abline(h=0); abline( h=lRes$dfPed$thr[pp], col="red")
  }
  
  # assess autocorrelation
  fnRep("assess autocorrelation")
  mAutoCor <- lRes$mAutoCor
  par( mfrow=fnCalcPanelPlotDim(ncol(mAutoCor)) )
  sX <- 'lag'
  sY <- 'Autocorrelation'
  for(sCol in colnames(mAutoCor)) {
    plot( mAutoCor[,sCol], main=paste("Autocorrelation",sCol,sep='\n'), xlab=sX, ylab=sY ); abline(h=0)
  }
  
  sFileLiaSample <- "pedigreePosteriorLiaSample.tsv"
  fnRep( paste( "Write draws from pedigree's posterior liability to file -", sFileLiaSample ))
  write.table( lRes$mTotLiaSample, sFileLiaSample, quote=F, sep="\t", row.names=F, col.names=T )
  
  llRes[[rr]] <- lRes
}


# get ped info with risks
dfPedRisk <- llRes[[1]]$dfPedRisk

# calc std dev on risk estimates
if( length(llRes)>1){
  # collate risks
  mRisk <- NA; mRisk <- sapply(llRes, function(dfRes2) dfRes2$dfPedRisk$risk )
  # risk mean and std dev
  vRiskMean <- apply( mRisk, 1, mean)
  vRiskSd <- apply( mRisk, 1, sd)
  dfPedRisk$risk <- vRiskMean
  dfPedRisk$riskStdDev <- vRiskSd
  
  # collate risks
  mRisk <- NA; mRisk <- sapply(llRes, function(dfRes2) dfRes2$dfPedRisk$nYearRisk )
  # risk mean and std dev
  vRiskMean <- apply( mRisk, 1, mean)
  vRiskSd <- apply( mRisk, 1, sd)
  dfPedRisk$nYearRisk <- vRiskMean
  dfPedRisk$nYearRiskStdDev <- vRiskSd
}

fnRep("report risk predictions")
dfPedRisk

sFileRes <- "results.tsv"
fnRep( paste("Writing pedigree with risk predictions to file -", sFileRes ) )
write.table( dfPedRisk, file = sFileRes, sep = "\t", quote = FALSE, row.names = FALSE )

fnRep("plot risk predictions")
par(mfrow=c(1,1))
fnBarPlotRisk(dfPedRisk)

if(F){
dfPedRiskAgg <- NULL
for( ii in 1:length(llRes)){
  dfPedRisk <- llRes[[ii]]$dfPedRisk
  dfPedRisk$ii <- ii
  dfPedRiskAgg <- rbind(dfPedRiskAgg, dfPedRisk)
}
#dfPedRiskAgg
}

if(bPlotToFile) dev.off(iDevFilePlot)



fnRep("Completed Successfully")
