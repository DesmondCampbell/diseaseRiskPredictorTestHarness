# diseaseRiskPredictorTestHarness
Command line program and test harnesses for the `diseaseRiskPredictor` package.

This repository contains a number of command line programs associated with the R `diseaseRiskPredictor` package.
These are 

* MultifactorialDiseaseRiskCalculator.R
* CategoricalRiskFactorTestHarness.R
* DisFileReaderTestHarness.R
* PedigreeTestHarness.R

Also included are a set of explanatory Word documents, and also some helper R scripts and test case directories.

## MultifactorialDiseaseRiskCalculator.R
This is a command line program for predicting multifactorial disease risk. The user supplies 

* a disease pedigree / family tree
* a disease model

The progam predicts risk disease for each pedigree member, according to the disease model. 
The program is an R script, it makes extensive use of the `diseaseRiskPredictor` package to do this, and provides a template for use of this package.

## Test Harnesses
There are three test harness programs used for exercising various aspects of the  diseaseRiskPredictor package functionality.
These work in the same way, namely there are a set of test cases (valid or invalid). The test harness is applied to each test case in turn. 
Associated with each test harness is a Word document explaining how to use it.

## Installing diseaseRiskPredictor
The R package `diseaseRiskPredictor` is described in its vignette. This includes details for how to install it. See 

* https://rawgit.com/DesmondCampbell/diseaseRiskPredictor/master/vignettes/diseaseRiskPredictor.html

