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
  cat( "USAGE: ./categoricalRiskFactorTestHarness.R <lifetime risk> <string of risk factor strata frequencies> <string of risk factor strata relative risks>\n")
  ###   e.g. ./categoricalRiskFactorTestHarness.R 0.154 "0.9 0.09 0.01" "1 9 10"
}



#### MMMM ##### program starts here ##############################################

### Command line argument parsing
bCmdLine <- !interactive()
if( bCmdLine) {
  vsArgs <- commandArgs(TRUE)
  options(width=200)
  
} else {
  cat("WARNING: INPUTS ARE HARD CODED !!!!!!!!!\n")
  sWD <- "C:/Desmond/diseaseRiskPredictor/testHarness"
  setwd(sWD)
  
  vsArgs <- c(0.9, "0.9 0.09 0.01", "1 9 10")
  #vsArgs <- c( 0.154, "0.9 0.0999999 0.0000001", "1 9 10")
  #vsArgs <- c( 0.154, "0.9 0.1", "1 9")
  vsArgs <- c( 0.1, "0.9 0.09 0.01", "8 2 1")
}
fnRep("Inputs are")
print(vsArgs)

if(length(vsArgs) != 3) {
  fnUsage()
  stop("bad command line")
}

# read inputs into appropriate variables
### !!!No checking for validity!!!
aa <- 0
sLifetimeRisk <- vsArgs[aa <- aa + 1 ]
sStratumFreq <- vsArgs[aa <- aa + 1 ]
sRelRisk <- vsArgs[aa <- aa + 1 ]

fnRep("User given inputs:")
cat("Lifetime risk: ", sLifetimeRisk, "\n")
cat("Frequencies: ", sStratumFreq, "\n")
cat("Relative risks: ", sRelRisk, "\n")

# convert inputs from strings to appropriate types
lifetimeRisk <- as.numeric(sLifetimeRisk)
vStratumFreq <- as.numeric(unlist(strsplit(sStratumFreq, split = " ")))
vRelRisk <- as.numeric(unlist(strsplit(sRelRisk, split = " ")))



# validate inputs and convert into data.frame
fnRep("run fnValidateCategoricalRiskFactorInputs()")
lRF <- fnValidateCategoricalRiskFactorInputs( lifetimeRisk, vStratumFreq, vRelRisk )
fnStr(lRF)



bPlotToFile <- !interactive()
if(bPlotToFile){
  sFilePlot <- format(Sys.time(), "plot-%X-%d-%b-%Y.pdf")
  sFilePlot <- gsub(":",".",sFilePlot) # XXXX colon is an illegal character in windows file names
  sFilePlot <- paste0("trace-", sFilePlot, sep = "")
  cat("INFO: writing convergence plot to file -", sFilePlot, "\n")
  pdf(sFilePlot, width = 10)
}

# calc solution
nofAttempts <- 0
dfLog <- NULL # global required for fnRunOptim()
dfRF <- lRF$dfRF
lControl <- list(trace=0, maxit=10000)
sMethod <- ifelse( nrow(dfRF)<=2,"BFGS","Nelder-Mead")

fnRep("run fnRunOptim() - First attempt")
nofAttempts <- nofAttempts + 1
lOptimise <- fnRunOptim( lifetimeRisk, dfRF, sMethod, lControl )
fnStr(lOptimise)
# plot showing optimiser converging
plotRefStraMean(lOptimise$lOptim$sMethod, vsArgs, lOptimise$lOptim$dfLog)

if( !is.null(lOptimise$sErrMsg) & lOptimise$lOptim$sMethod!="BFGS" ) {
  # 2nd attempt
  fnRep("run fnRunOptim() - 2nd attempt")
  nofAttempts <- nofAttempts + 1
  sMethod <- "BFGS"
  lOptimise <- fnRunOptim( lifetimeRisk, dfRF, sMethod, lControl )
  fnStr(lOptimise)
  # plot showing optimiser converging
  plotRefStraMean(lOptimise$lOptim$sMethod, vsArgs, lOptimise$lOptim$dfLog)
}



cat("\n");
fnRep("The solution is given below")
dfSolution <- lOptimise$dfRF
dfSolution$method <- lOptimise$lOptim$sMethod
dfSolution$nofAttempts <- nofAttempts
dfSolution$SOLUTION <- "SOLUTION"
vsColId <- c("SOLUTION","method","nofAttempts")
dfSolution <- dfSolution[,c( vsColId, setdiff(colnames(dfSolution),vsColId) )]
print(dfSolution)

cat("\n");
fnRep("Diagnostics on how good a solution it is are given below")
dfDiagnostics <- lOptimise$dfRFDiagnostics
dfDiagnostics$DIAGNOSTICS <- "DIAGNOSTICS"
vsColId <- c("DIAGNOSTICS")
dfDiagnostics <- dfDiagnostics[,c( vsColId, setdiff(colnames(dfDiagnostics),vsColId) )]
print(dfDiagnostics)
if( !is.null(lOptimise$sErrMsg) ){
  fnRep("Error message returned by the optimiser is given below")
  cat("ERROR:", lOptimise$sErrMsg, "\n")
}



cat("\n");
sFileSol <- "CategoricalRiskFactorTestHarness.out.tsv"
fnRep( paste("Solution written to file -", sFileSol ))
write.table( lOptimise$dfRF, sFileSol, row.names=F, quote=F, sep="\t" )

if(bPlotToFile) dev.off()

fnRep("Completed Successfully")
