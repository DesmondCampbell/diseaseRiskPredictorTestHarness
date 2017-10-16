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

# reload library
sPkg <- "diseaseRiskPredictor"
suppressPackageStartupMessages( library(sPkg,char=T) )
if(T){ # overwrite kinship2::plot.pedigree()
	fnRep( "patch kinship2::plot.pedigree()")
	assignInNamespace( "plot.pedigree", diseaseRiskPredictor:::plot.pedigree.FIXED, ns="kinship2")
}



fnUsage <- function(){
	cat( "USAGE: ./PedigreeTestHarness.R <pedFilePath>\n")
	###   e.g. ./PedigreeTestHarness.R ped.p2c2.tsv
}



#### MMMM ##### program starts here ##############################################

bCmdLine <- !interactive()
if( bCmdLine) {
	vsArgs <- commandArgs(TRUE)
	options(width=200)
	
} else {
	cat("WARNING: INPUTS ARE HARD CODED !!!!!!!!!\n")
	sWD <- "C:/Desmond/diseaseRiskPredictor/testHarness"
	setwd(sWD)
	
	vsArgs <- c("ValidatePedigree/twoTrios.ped")
	vsArgs <- c("ValidatePedigree/unrelatedIndividual.ped")
	vsArgs <- c("ValidatePedigree/bad/personIsTheirOwnAncestor.ped")
	vsArgs <- c("ValidatePedigree/good/grandmaOfHerOwnHusband.ped")
	vsArgs <- c("ValidatePedigree/ped.p2c3.min.tsv")
	vsArgs <- c("pedigrees/ped.p2c.multipleTwins.tsv")
	vsArgs <- c("pedigrees/ped.tmp.tsv")
}
fnRep("Inputs are")
print(vsArgs)



if(length(vsArgs) != 1) {
	fnUsage()
	stop("bad command line")
}

# read inputs into appropriate variables
aa <- 0
sFilePed <- vsArgs[ aa <- aa + 1 ]



fnRep( paste("Read pedigree from file -", sFilePed ))
# read data from file
dfPed <- NA; dfPed <- read.table( sFilePed, header=T, stringsAsFactors=F)
fnRep( "Pedigree is" )
dfPed

fnRep( "run fnValidatePedigree()" )
dfPed <- fnValidatePedigree( dfPed )
fnStr(dfPed)

fnRep( "run fnConstructPedigree()" )
oPedigree <- NA; oPedigree <- fnConstructPedigree( dfPed )
fnStr(oPedigree)

# creates a file for any plots to go into
# rely on process death to close the plot file
bPlotToFile <- !interactive()
if(bPlotToFile) {
	sFilePlot <- "PedigreeTestHarness.R.plots.pdf"
	fnRep("writing plots to file -", sFilePlot)
	pdf(sFilePlot, paper="a4r")
	iDevFilePlot <- dev.cur()
}

fnRep( "run plot()" )
oPlotPed <- plot(oPedigree)
title(sFilePed)
fnStr(oPlotPed)

if(bPlotToFile) dev.off(iDevFilePlot)



fnRep("Completed Successfully")
