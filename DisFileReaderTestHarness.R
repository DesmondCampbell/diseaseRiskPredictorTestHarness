#!/usr/bin/env Rscript



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



fnUsage <- function(){
  print( "USAGE: ./DisFileReaderTestHarness.R <diseaseModelFilePath>\n")
  ###   e.g. ./DisFileReaderTestHarness.R ValidateDiseaseModel/diseaseModel4.can.E.tsv
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
  
  cat("WARNING: INPUTS ARE HARD CODED !!!!!!!!!\n")
  #vsArgs <- "dm.marajana.txt"
  vsArgs <- "ValidateDiseaseModel/diseaseModel.a60.p5.tsv"
  vsArgs <- "ValidateDiseaseModel/dm.marajana2.txt"
  vsArgs <- "ValidateDiseaseModel/dm.SZ2.tsv"
  vsArgs <- "ValidateDiseaseModel/dm.AOO.tsv"
  vsArgs <- "ValidateDiseaseModel/dm.MD1.tsv"
  vsArgs <- "ValidateDiseaseModel/dm.SZ1.tsv"
  vsArgs <- "ValidateDiseaseModel/dm.Con2.tsv"
  vsArgs <- "ValidateDiseaseModel/dm.pgs.txt"
  vsArgs <- "ValidateDiseaseModel/dm.MD.sexPgsCannabis.txt"
  vsArgs <- "test/dm.Con.Qua.txt"
  vsArgs <- "test/dm.Con.Cat.txt"
  vsArgs <- "test/dm.depression.txt"
  vsArgs <- "test/dm.MD.sexPgs.txt"
  vsArgs <- "test/dm.MD1.txt"
  vsArgs <- "test/dm.multiAoo.txt"
  vsArgs <- "test/dm.schizophrenia.txt"
  vsArgs <- "test/dm.sexPgsCannabis.txt"
  vsArgs <- "ValidateDiseaseModel/dm.Con1.txt"
  vsArgs <- "diseaseModels/dm.depression.txt"
}
fnRep("Inputs are")
print(vsArgs)

if(length(vsArgs) != 1) {
  fnUsage()
  stop("bad command line")
}

# read inputs into appropriate variables
aa <- 0
sFileDis <- vsArgs[ aa <- aa + 1 ]


dfLog <- NULL # global required for fnRunOptim()

fnRep( paste("Read disease model from file -", sFileDis ))
lDis <- NA; lDis <- fnReadFileDis( sFileDis )
fnRep( "Disease model is" )
lDis
if(!is.null(lDis$sErrMsg)){
  stop(lDis$sErrMsg)
}

fnRep("Completed successfully")
