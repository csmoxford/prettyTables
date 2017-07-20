

ConstructFlexTable$methods(
  SetHeaderRowByIndex = function(index, value = TRUE) {
    "Assign header rows by row index"
    isHeaderRow[index] <<- value
  }
)

ConstructFlexTable$methods(
  SetHeaderRowAll = function(value = TRUE) {
    "Set all rows to header rows or not"
    isHeaderRow[] <<- value
  }
)
