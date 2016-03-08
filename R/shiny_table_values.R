#' Table values shiny app
#'
#' An assistance too to use the \code{\link{table_values}} function for generating summary tables. The shiny app send the code to repeat the generation of the tables to the console or a seperate log file. This log file can be rerun at a later date without the need to use the app again. There are options to output the tables to a word document using the RTF package.
#'
#' \strong{Load data} tab:
#'
#' If you wish to log to an external file type a file path and file name (.R file to open in R) here and start logging. Type or paste the path to the data file you want to generate a table from. Select the specific data file from the list and load the data. The program should be able to cope with csv and stata data files. Note that you can use backspace and search for the file you want to use. The dataset will be shown if it loads correctly.
#'
#' \strong{Stratify} tab:
#'
#' Select the stratification variable if required. You can rename the strata here using a named list of values. See the example for the format. There are options to include strata counts in the column names and an overall column.
#'
#' \strong{Add/remove variables} tab:
#'
#' One by one select the variable you want to summarise. You can enter variable names if different to the columns names in the dataset. Choose the display format and number of significant figures for the numeric summaries. Add each one using submit. A summary of your selections is displayed in the Tables tab on the right. You can build the table as you go along to see what it looks like. You can delete variable if you make a mistake and change the order of the variables.
#'
#' \strong{Misc} tab:
#'
#' These are two variables here. The first allows you to choose whether to display column counts by strata by the number of observations, number of missing values or not display either. The second pre-allocates to the data.frame and will only need to be increased if more than 100 rows are needed in the table.
#'
#' \strong{Build Table} tab:
#'
#' This tab pushes the table to a word document and severs the connection to the log file. Enter a path and file name (.doc) of where to generate the word document. Note this will overwrite existing documents and will error if the file is already open when you try to run this. To push the table to word you will want to click the Open RTF connection, Push table to word, and then Close RTF connection.

# required by the shiny app
#' @import rtf
#' @import shiny
#' @importFrom readstata13 read.dta13

shiny_table_values <- function(){
  shiny::runApp(file.path(system.file("Shiny/prettyTables_gui",package = "prettyTables")),launch.browser=TRUE)
}
