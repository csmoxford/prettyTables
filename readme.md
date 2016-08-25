# prettyTables

This is an R package dedicated to creating publication ready tables. Provided are a number of utilities designed to assist in the creation of clean tidy tables. There is also a shiny application provided which aims to make creating a clinical trial baseline table easy.

## Installation

### R
```r
library(devtools)
install_github("csmoxford/prettyTables")
```

### Stata
Stata requires the package to be installed in R, for R to be available from the command line and for the prettyTables command to be installed for use in Stata.

1. R must be available on the computers cmd PATH. Test by running this .bat file: `inst/shiny/prettyTables.bat`:
If this fails only you need to do the following:

    1. Locate the folder containing the R executables choosing the latest version you have installed and the x64 bit if you have it. (Mine is this: C:\Program Files\R\R-3.3.0\bin\x64)
    2. Copy this folder path
    3. Go to start, right click on computer and select properties
    4. Select 'Advanced System Setting'
    5. Select 'Environment variables'
    6. In the upper box look for a Variable called PATH (if there isn't one create it in all capitals)
    7. Click edit scroll to the end of what is currently there. Add a semi colon and then paste the path to your R folder. Ok. Ok and all done. Test again.
3. Copy the `inst/shiny/prettyTables.ado` file
4. Open Stata and type the command sysdir
5. Go to the folder listed under personal (you may need to create this folder)
6. Paste the prettyTables.ado file to this folder
7. close all instances of Stata
8. Open Stata and type the command prettyTables (this should open a blue window followed shortly by a webpage in your default browser)

## Usage

```r
library(prettyTables)
?prettyTables
shiny_table_values()
```

The command to launch the user interface for Stata is `prettyTables`.


## History
See the prettyTables changelog

## Credits
Programmed by: Peter Dutton (Centre for Statistic in Medicine, University of Oxford)
## License
GPL-2