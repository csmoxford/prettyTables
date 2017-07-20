ConstructFlexTable$methods(
  SetFontSize = function(size) {
    "Set the fontsize of all cells. size is a numeric value"
    fontSize[,] <<- size
  }
)

ConstructFlexTable$methods(
  SetFontColorAll = function(color) {
    "Set the fontColor of all cells. color should be a hex color."
    fontColor[,] <<- color
  }
)

ConstructFlexTable$methods(
  SetFontColorRowAlternating = function(colors) {
    "Set the fontColor alternating by colors. color should be a vector of hex colors."
    i = 1
    j = 0
    while(i <= numRow) {
      fontColor[i,] <<- colors[j+1]
      j = (j + 1) %% length(colors)
      i = i+1
    }
  }
)

ConstructFlexTable$methods(
  SetFontColorRowAlternatingByMatching = function(colors, matching) {
    "Set the font color, alternating colors by matching group of rows. A set of rows is colored together if matching is the same for consecutive rows. colors should be a vector of hex colors."
    if(length(matching) != numRow) {
      stop("matching must be length numRow (",numRow, ")")
    }
    i = 1
    j = 0
    while(i <= numRow) {
      fontColor[i,] <<- colors[j+1]
      if(i < numRow){
        if(matching[i] != matching[i+1]) {
          j = (j + 1) %% length(colors)
        }
      }
      i = i + 1
    }
  }
)

ConstructFlexTable$methods(
  SetFontStyleByRow = function(style = "bold", index) {
    "Set the font style by row. Style should be one of 'normal', 'italic', 'bold'. Only the first letter is currently used so shorthand is possible."
    for(i in index) {
      fontStyle[i,] <<- rep(style,numCol)
    }
  }
)
