


ConstructFlexTable$methods(
  SetStyleVanilla = function(adjustRowSpan = FALSE) {
    "This function assumes a single header row is present and mimics the vanilla.table function in ReporteRs"
    isHeaderRow[1] <<- TRUE
    isHeaderRow[2:numRow] <<- FALSE
    if(adjustRowSpan) {
      rowSpan[,] <<- 1
      colSpan[,] <<- 1
    }
    borderWidthVertical[,] <<- 0
    borderWidthHorizontal[,] <<- 0
    SetBorderWidthHorizontalByRow(3,c(0,numRow))
    SetBorderWidthHorizontalByRow(2,c(1))
    textAlign[,] <<- "right"
    fontColor[,] <<- "#000000"
    fontSize[,] <<- 11
    fontStyle[,]  <<- "normal"
    padding[,] <<- 0

  }
)
