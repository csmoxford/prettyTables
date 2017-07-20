

ConstructFlexTable$methods(
  SetSpanRowByRange = function(j, from, to) {
    "On column j sets rowSpan between from and to."
    rowSpan[from:to,j] <<- 0
    rowSpan[from,j] <<- to - from + 1
  }
)

ConstructFlexTable$methods(
  SetSpanRowByMatching = function(j, matching) {
    "On column j sets rowSpan based on matching. Consecutive values in matching which are the same are merged."
    if(length(matching) != numRow) {
      stop("matching must be length numRow (",numRow, ")")
    }
    rowIndex = 1
    i = 2
    counter = 1
    while(i <= numRow) {
      if(matching[rowIndex] == matching[i]) {
        rowSpan[i,j] <<- 0
        counter = counter + 1
      } else {
        rowSpan[rowIndex,j] <<- counter
        counter = 1
        rowIndex = i
      }
      i = i + 1
    }
    rowSpan[rowIndex,j] <<- counter
  }
)

ConstructFlexTable$methods(
  SetSpanRowByColumn = function(j) {
    "On column j sets rowSpan based on the data. Consecutive values in the column which are the same are merged."
    SetSpanRowByMatching(j, data[,j])
  }
)

ConstructFlexTable$methods(
  SetSpanColumnByRange = function(i, from, to) {
    "On row i sets colSpan between from and to."
    colSpan[i,from:to] <<- 0
    colSpan[i,from] <<- to - from + 1
  }
)

ConstructFlexTable$methods(
  SetSpanColumnByMatching = function(i,matching) {
    "On row i sets colSpan based on matching. Consecutive values in matching which are the same are merged."
    if(length(matching) != numCol) {
      stop("matching must be length numCol (",numCol, ")")
    }
    colIndex = 1
    j = 2
    counter = 1
    while(j <= numCol) {
      if(matching[colIndex] == matching[j]) {
        colSpan[i,j] <<- 0
        counter = counter + 1
      } else {
        colSpan[i,colIndex] <<- counter
        counter = 1
        colIndex = j
      }
      j = j + 1
    }
    colSpan[i, colIndex] <<- counter
  }
)

ConstructFlexTable$methods(
  SetSpanColumnByRow = function(j) {
    "On column j sets colSpan based on the data. Consecutive values in the row which are the same are merged."
    SetSpanColumnByMatching(j, data[j,])
  }
)
