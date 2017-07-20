


ConstructFlexTable$methods(
  CutRows = function(index) {
    "Remove rows by index updating all the property parameters in the process"
    index = sort(index, decreasing = TRUE)
    for(i in index) {
      data <<- data[-i,]

      numRow <<- numRow - 1
      isHeaderRow <<- isHeaderRow[-i]
      # update the rowSpan in a sensible way when a spanned row is removed.
      for(j in 1:numCol){
        if(rowSpan[i,j] == 0){
          iStar = i
          total = 0
          while(rowSpan[iStar,j] == 0 & iStar <= numRow) {
            total = total + 1
            rowSpan[iStar,j] <<- 1
            iStar = iStar + 1
          }
          iStar = i-1
          while(rowSpan[iStar,j] == 0){
            iStar =  iStar - 1
          }
          rowSpan[iStar,j] <<- rowSpan[iStar,j] - total
        } else if(rowSpan[i,j] > 1){
          for(iStar in i + 1:(rowSpan[i,j]-1)) {
            rowSpan[iStar,j] <<- 1
          }
        }
      }
      ####
      rowSpan <<- rowSpan[-i, ]
      colSpan <<- colSpan[-i, ]
      color <<- color[-i, ]
      borderWidthVertical <<- borderWidthVertical[-i, ]
      borderWidthHorizontal <<- borderWidthHorizontal[-i, ]
      textAlign <<- textAlign[-i, ]
      fontColor <<- fontColor[-i, ]
      fontSize <<- fontSize[-i, ]
      fontStyle  <<- fontStyle[-i, ]
      padding <<- padding[-i, ]
    }
  }
)


ConstructFlexTable$methods(
  CutColumns = function(index) {
    "Remove columns by index updating all the property parameters in the process"
    index = sort(index, decreasing = TRUE)
    for(j in index) {
      data <<- data[,-j]

      numCol <<- numCol - 1
      # update the colSpan in a sensible way when a spanned column is removed.
      for(i in 1:numRow){
        if(colSpan[i,j] == 0){
          jStar = j
          total = 0
          while(colSpan[i,jStar] == 0 & jStar <= numCol) {
            total = total + 1
            colSpan[i,jStar] <<- 1
            jStar = jStar + 1
          }
          jStar = j-1
          while(colSpan[i,jStar] == 0){
            jStar =  jStar - 1
          }
          print(c(i,jStar, total))
          colSpan[i,jStar] <<- colSpan[i,jStar] - total
        } else if(colSpan[i,j] > 1){
          for(jStar in j + 1:(colSpan[i,j]-1)) {
            colSpan[i,jStar] <<- 1
          }
        }
      }
      ####
      rowSpan <<- rowSpan[, -j]
      colSpan <<- colSpan[, -j]
      color <<- color[, -j]
      borderWidthVertical <<- borderWidthVertical[, -j]
      borderWidthHorizontal <<- borderWidthHorizontal[, -j]
      textAlign <<- textAlign[, -j]
      fontColor <<- fontColor[, -j]
      fontSize <<- fontSize[, -j]
      fontStyle  <<- fontStyle[, -j]
      padding <<- padding[, -j]
    }
  }
)
