
*************************************************************************
* drop a version if it exists
capture program drop prettyTables
*************************************************************************
* sourcing program program
*************************************************************************
program define prettyTables
	shell Rscript "I:/Data/Useful Programs/R/prettyTables-package/prettyTables/inst/shiny/prettyTables_gui/run.r"
end
