*************************************************************************
* Program to launch the prettyTables user interface from within Stata
*************************************************************************
* drop a version if it exists
capture program drop prettyTables
*************************************************************************
* sourcing program program
*************************************************************************
program define prettyTables
	shell Rscript -e "library(prettyTables); shiny_table_values()"
end

