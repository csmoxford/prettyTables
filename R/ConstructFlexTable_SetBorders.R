ConstructFlexTable$methods(
  SetBorderWidthVertical = function(width = 2) {
    "Set the Border width for vertical borders globally"
    borderWidthVertical[,] <<- width
  }
)

ConstructFlexTable$methods(
  SetBorderWidthVerticalByColumn = function(width = 2, index) {
    "Set the Border width for vertical borders globally. Note an index of zero is the left most border and an index of numCol+1 is the rightmost border"
    for(j in index){
      borderWidthVertical[,j + 1] <<- width
    }
  }
)

ConstructFlexTable$methods(
  SetBorderWidthVerticalByMatching = function(width = 2, matching = NULL){
    "Set the border width of borders according to matching. Will place borders around consecutively identical values of matching"
    if(length(matching) != numCol) {
      stop("matching must be length numCol (",numCol, ")")
    }
    borderWidthVertical[,1] <<- width
    j = 2
    while(j <= numCol) {
      if(matching[j-1] != matching[j]) {
        borderWidthVertical[,j] <<- width
      }
      j = j + 1
    }
    borderWidthVertical[,numCol + 1] <<- width
  }
)

ConstructFlexTable$methods(
  SetBorderWidthVerticalByMatchingOnRow = function(width = 2, i){
    "A wrapper for SetBorderWidthVerticalByMatching. Matches using data stored in the class with column index"
    SetBorderWidthVerticalByMatching(width, data[i,])
  }
)

ConstructFlexTable$methods(
  SetBorderWidthHorizontal = function(width = 2){
    "Set the Border width for horizontal borders globally"
    borderWidthHorizontal[,] <<- width
  }
)

ConstructFlexTable$methods(
  SetBorderWidthHorizontalByRow = function(width = 2, index) {
    "Set the Border width for vertical borders globally. Note an index of zero is the left most border and an index of numCol+1 is the rightmost border"
    for(i in index){
      borderWidthHorizontal[i + 1,] <<- width
    }
  }
)

ConstructFlexTable$methods(
  SetBorderWidthHorizontalByMatching = function(width = 2, matching = NULL){
    "Set the border width of borders according to matching. Will place borders around consecutively identical values of matching"
    if(length(matching) != numRow) {
      stop("matching must be length numRow (",numRow, ")")
    }
    borderWidthHorizontal[1,] <<- width
    i = 2
    while(i <= numRow) {
      if(matching[i-1] != matching[i]) {
        borderWidthHorizontal[i,] <<- width
      }
      i = i + 1
    }
    borderWidthHorizontal[numRow + 1,] <<- width
  }
)

ConstructFlexTable$methods(
  SetBorderWidthHorizontalByMatchingOnColumn = function(width = 2, j){
    "A wrapper for SetBorderWidthHorizontalByMatching. Matches using data stored in the class with column j"
    SetBorderWidthHorizontalByMatching(width, data[,j])
  }
)
