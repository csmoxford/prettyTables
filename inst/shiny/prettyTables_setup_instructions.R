
# First time setup install package dependencies
install.packages(c("shiny", "read.dta13", "rtf"))
## Packages prettyTables depends on
# Shiny: Is a graphical user interface package which allows the user to create interactive webpages which are driven by R
# foreign: contains the function read.dta for reading stata data files directly into R
# rtf: Is an output package for creating rich text formatted documents. This can take text, tables and graphs and output to rtf. If this document is given the .doc extension Word will automatically open them
##########################################################

# Install or update prettyTables from the I drive
install.packages("I:/Data/Peter_Dutton/Shared_code/R/prettyTables_1.0.tar.gz", repos = NULL)
##########################################################

##########################################################
## stata setup:
# 1. R must be available on the computers cmd PATH
# Test by running the following .bat file:
# I:\Data\Peter_Dutton\Shared_code\R\prettyTables\inst\shiny\prettyTables.bat
## If this fails only you need to do the following:
## a. Locate the folder containing the R executables choosing the latest version you have installed and the x64 bit if you have it.
## (Mine is this: C:\Program Files\R\R-3.1.3\bin\x64)
## b. Copy this folder path
## c. Go to start, right click on computer and select properties
## d. Select 'Advanced System Setting'
## e. Select 'Environment variables'
## f. In the lower box look for a Variable called Path
## g. Click edit scroll to the end of what is currently there. Add a semi colon and then paste the path to your R folder. Ok. Ok and all done

# 2. Go here: I:\Data\Peter_Dutton\Shared_code\R\prettyTables\inst\shiny
# 3. Copy the prettyTables.ado file
# 4. Open stata and type the command sysdir
# 5. Go to the folder listed under personal (you may need to create this folder)
# 6. Paste the prettyTables.ado file to this folder
# 7. close all instances of Stata
# 8. Open Stata and type the command prettyTables (this should open a blue window followed shortly by a webpage in your default browser)
# If this does not work then come find me!!
##########################################################

##########################################################
## Using in stata
# Type the command prettyTables in stata to run from stata
# Note that you need to close the webpage or shell in order to continue using stata (stata is waiting for the command to complete...)
##########################################################


##########################################################
## Using in R
library(prettyTables)
shiny_table_values()
# Note that you need to close the webpage or shell in order to continue using R (R is waiting for the command to complete...)
##########################################################


##########################################################
## Using from batch (creating a new independent R session)
# there is an executable in .bat form here:
# I:\Data\Peter_Dutton\Shared_code\R\prettyTables\inst\shiny\prettyTables.bat
##########################################################

##########################################################
## Other notes
##########################################################
# There is an option to create a log file. This file creates an R script of the commands pushed to it. It saves the following:
# 1. Any data loads
# 2. Open a connection to the word document
# 3. Push tables to the word document
# 4. Close the connection to the word document
# This R script can then be run from stata in a similar way to calling a do file:
# shell Rscript "(script path and name here)"
