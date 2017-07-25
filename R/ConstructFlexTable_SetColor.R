ConstructFlexTable$methods(
  SetColorAll = function(color) {
    "Set the background color of all cells. Color should be a hex color."
    color[,] <<- color
  }
)

ConstructFlexTable$methods(
  SetColorRowAlternatingRow = function(colors) {
    "Set the background color alternating colors by row. Colors should be a vector of hex colors."
    i = 1
    j = 0
    while(i <= numRow) {
      color[i,] <<- colors[j+1]
      j = (j+1) %% length(colors)
      i = i + 1
    }
  }
)

ConstructFlexTable$methods(
  SetColorRowAlternatingByMatching = function(colors, matching) {
    "Set the background color alternating colors by matching group of rows. A set of rows is colored together if matching is the same for consecutive rows. Colors should be a vector of hex colors."
    if(length(matching) != numRow) {
      stop("matching must be length numRow (",numRow, ")")
    }
    i = 1
    j = 0
    while(i <= numRow) {
      color[i,] <<- colors[j+1]
      if(i < numRow) {
        if(matching[i] != matching[i+1]){
          j = (j+1) %% length(colors)
        }
      }
      i = i + 1
    }
  }
)

ConstructFlexTable$methods(
  SetColorRowAlternatingByColumn = function(colors, j) {
    "Set the background color alternating colors by matching group of rows. Consecutive values in the row which are the same are considered a group. Colors should be a vector of hex colors."
    if(is.null(j)) {
      stop("Expecting three parameters : colors, j")
    }
    SetColorRowAlternatingByMatching(colors, data[,j])
  }
)

ConstructFlexTable$methods(
  SetColorRowByIndex = function(color, index) {
    "Set the background color of a specific row. Color should be a hex color."
    if(is.null(index)) {
      stop("Expecting three parameters : colors, index")
    }
    for(i in index) {
      color[i,] <<- color
    }
  }
)

ConstructFlexTable$methods(
  SetColorColumnByIndex = function(color, index) {
    "Set the background color of a specific column. Color should be a hex color."
    if(is.null(index)) {
      stop("Expecting three parameters : colors, index")
    }
    for(j in index) {
      color[,j] <<- color
    }
  }
)

