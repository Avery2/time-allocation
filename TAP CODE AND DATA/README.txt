This zip file contains data and code used in:
'Work time, market integration, and the original affluent society'
Rahul Bhui, Maciej Chudek, and Joseph Henrich

The analysis code in the main folder will read in the data and run the main analyses.

folders
-------
main:
 -tap_SS_individual_plots.r: code for analyzing data at individual level
 -tap_SSLS_society_plots.r: code for analyzing data at society level
 -tap_supplemental.r: code for analyses reported in supporting information

data: contains data
 -OECD_1564_TUSupdatePortal.csv: OECD data
 -TAPoecdenergy.csv: energy costs of OECD time use categories
 -TAPOriginal.csv: raw small-scale society data
 -TAPpostproc.csv: small-scale society data post-processing
 -TAPworkdefEn.csv: definitions of work
data/AUXIL: auxiliary data on individual ages

out: results and figures saved here
 -TAP_hierarchical_bayesian_regressions.rds: individual-level analysis results
 -TAPoecd_bayesian_regressions.rds: society-level analysis results

pre-processing: code for processing and loading small-scale society data; will be called by main analysis code, do not need to run manually
 -load_tap.r: code for loading small-scale society data
 -preprocess_tap.r: code for pre-processing small-scale society data