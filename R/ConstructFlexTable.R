#' Construct Flex Table
#'
#' A reference class used to prepare data and formatting to create a \code{\link{FlexTable}} object in the \code{\link{ReporteRs}} package. This class has a large number of modifier functions aimed at making tables quicker and easier than the functions provided for the FlexTable class.
#'
#' This requires ReporteRs to be installed and ReporteRs requires a java JDK to be installed to work correcly.
#'
#' @field data The data to display
#' @field numRow The number of rows in data
#' @field numCol The number of columns in data
#' @field isHeaderRow A logical vector of length numRow. Is used to set header rows in FlextTable instead of adding to body.
#' @field rowSpan A numRow*numCol matrix denoting row spans. The number in the cell denotes the number of rows to span. Zero is entered when a cell is spanned and not visible. Note: can only by 1*m and not n*m
#' @field colSpan A numRow*numCol matrix denoting column spans. The number in the cell denotes the number of columns to span. Zero is entered when a cell is spanned and not visible. Note: can only by n*1 and not n*m
#' @field color A numRow*numCol matrix of hex colors. Used for background colour of cells.
#' @field borderWidthVertical A (numRow+1)*(numCol+1) matrix of vertical border edge widths.
#' @field borderWidthHorizontal A (numRow+1)*(numCol+1) matrix of horizontal border edge widths.
#' @field textAlign A numRow*numCol matrix of text alignment. One of 'left', 'right', 'center', 'justify'.
#' @field fontStyle A numRow*numCol matrix of font style. One of 'normal', 'italic', 'bold'. Only the first letter is currently used so shorthand is possible.
#' @field fontColor A numRow*numCol matrix of font colour. These should be hex colors.
#' @field fontSize A numRow*numCol matrix of font size (numeric).
#' @field padding A numRow*numCol matrix for the cell padding.
#'
#'
#' @details
#' This package will accept the data as a data.frame or matrix and convert all entries to text to avoid problems with interactions between factors, numerics and header rows. For this reason it is recommended that all post processing of numerics is completed before passing the data to the class.
#'
#' Because this is a reference class, autocomplete in Rstudio will display all the available functions and parameters by typing 'objectName'$. All the functions start with a capital letter (with the exception of r's built in methods) and are named in a structured manner. The first word of the function name indicates its purpose. The following names are used:
#' \itemize{
#' \item{\bold{Insert}}{ Insert additional rows, columns and header rows}
#' \item{\bold{Set}}{ Set parameter values for everything from spanning rows and columns to borders to text alignment}
#' \item{\bold{Cut}}{ Remove rows or columns}
#' \item{\bold{Get}}{ GetTable Calls a function to create the FlexTable}
#' }
#'
#' The copy command (\code{'objectName'$copy()}) may be used to clone the reference object. Reference classes by default refer to one version even if referenced from different variables so this method must be used if you want to duplicate and modify one version of the object only.
#'
#' All stored values start with lowercase. The data is stored in data and can be modified like a regular matrix or data.frame. We recommend changing non data values using the Set functions. This is particularly important for rowSpan and colSpan to ensure FlexTable does not throw errors or mess up intended. The Set functions are pretty flexible right now but if you feel there is something missing please let us know.
#'
#' @example inst/examples/ConstructFlexTable_iris.R
#'
#' @exportClass ConstructFlexTable
#' @export ConstructFlexTable
ConstructFlexTable = setRefClass(
  "ConstructFlexTable",
  fields = c(
    "data" = "ANY",
    "numRow" = "numeric",
    "numCol" = "numeric",
    "isHeaderRow" = "logical",
    "rowSpan"="matrix",
    "colSpan"="matrix",
    "color"="matrix",
    "borderWidthVertical"="matrix",
    "borderWidthHorizontal"="matrix",
    "textAlign"="matrix",
    "fontStyle"="matrix",
    "fontColor"="matrix",
    "fontSize"="matrix",
    "padding"="matrix"
  ),
  methods = c(
    new = function(dta){
      initialize(dta)
    },
    initialize = function(data, header = NULL){
      "Initialisation function. This is called by \\code{ConstructFlexTable()}. Pass the data and optionally a header row to create a new object. Header can either be a vector or TRUE (which takes the column names for data. Note that for matching the header will count as a row. For this reason it may be easier to generate a header row seperately or add later"
      data <<- data
      numRow <<- dim(data)[1]
      numCol <<- dim(data)[2]

      for(j in 1:numCol) { # avoid factors by converting to character.
        data[,j] <<- as.character(data[,j])
      }

      isHeaderRow <<- rep(FALSE,numRow)
      rowSpan <<- matrix(1,numRow,numCol)
      colSpan <<- matrix(1,numRow,numCol)
      color <<- matrix("#ffffff",numRow,numCol)
      borderWidthVertical <<- matrix(1,numRow+1,numCol+1)
      borderWidthHorizontal <<- matrix(1,numRow+1,numCol+1)
      textAlign <<- matrix("left", numRow, numCol)
      fontColor <<- matrix("#000000", numRow, numCol)
      fontSize <<- matrix(11, numRow, numCol)
      fontStyle  <<- matrix("normal", numRow, numCol)
      padding <<- matrix(0,numRow,numCol)
      if(!is.null(header)) {
        if(header[1] == TRUE) {
          InsertHeaderRow(0, names(data))
        } else {
          InsertHeaderRow(0, header)
        }
      }


    },
    GetTable = function(...) {
      "A wrapper function that calls GetFlexTable. Can pass multiple ConstructFlexTable objects into this function. the object this is called on goes first."
      return(GetFlexTable(.self, ...))
    },
    show = function() {
      "Currently prints the data.frame. I want this to include span information too!"
      print(data)
    }
  )
)

