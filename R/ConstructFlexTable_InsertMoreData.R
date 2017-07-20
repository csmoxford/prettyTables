
ConstructFlexTable$methods(
  InsertRow = function(afterIndex, values) {
    "Insert a row or number of rows after a given index. Using an index of 0 inserts at the top."
    run = TRUE
    if("data.frame" %in% class(values)){
      if(dim(values)[1] > 1) {
        for(i in 1:(dim(values)[1])){
          InsertRow(afterIndex + i - 1, values[i,])
          run = FALSE
        }
      } else {
        i <- sapply(values, is.factor)
        values[i] <- lapply(values[i], as.character)
        values = unlist(values, use.names = FALSE)
      }
    }
    if(run) {
      if(length(values) != numCol) {
        stop("Data to insert does not match number of columns")
      }

      if(afterIndex < 1) {
        data <<- rbind(values,data)

        isHeaderRow <<- c(FALSE, isHeaderRow)
        rowSpan <<- rbind(1, rowSpan)
        colSpan <<- rbind(1, colSpan)
        color <<- rbind("#ffffff", color)
        borderWidthVertical <<- rbind(1, borderWidthVertical)
        borderWidthHorizontal <<- rbind(1, borderWidthHorizontal)
        textAlign <<- rbind("left", textAlign)
        fontColor <<- rbind("#000000", fontColor)
        fontSize <<- rbind(11, fontSize)
        fontStyle  <<- rbind("normal", fontStyle)
        padding <<- rbind(0, padding)
      } else if(afterIndex >= numRow){
        data <<- rbind(data, values)

        isHeaderRow <<- c(isHeaderRow, FALSE)
        rowSpan <<- rbind(rowSpan, 1)
        colSpan <<- rbind(colSpan, 1)
        color <<- rbind(color, "#ffffff")
        borderWidthVertical <<- rbind(borderWidthVertical, 1)
        borderWidthHorizontal <<- rbind(borderWidthHorizontal, 1)
        textAlign <<- rbind(textAlign, "left")
        fontColor <<- rbind(fontColor, "#000000")
        fontSize <<- rbind(fontSize, 11)
        fontStyle  <<- rbind(fontStyle, "normal")
        padding <<- rbind(padding, 0)
      } else {
        data <<- rbind(data[1:afterIndex,], values, data[(afterIndex + 1):numRow,])

        isHeaderRow <<- c(isHeaderRow[1:afterIndex], FALSE, isHeaderRow[(afterIndex + 1):numRow])
        rowSpan <<- rbind(rowSpan[1:afterIndex,], 1, rowSpan[(afterIndex + 1):numRow,])
        colSpan <<- rbind(colSpan[1:afterIndex,], 1, colSpan[(afterIndex + 1):numRow,])
        color <<- rbind(color[1:afterIndex,], "#ffffff", color[(afterIndex + 1):numRow,])
        borderWidthVertical <<- rbind(borderWidthVertical[1:afterIndex,], 1, borderWidthVertical[(afterIndex + 1):(numRow + 1),])
        borderWidthHorizontal <<- rbind(borderWidthHorizontal[1:afterIndex,], 1, borderWidthHorizontal[(afterIndex + 1):(numRow + 1),])
        textAlign <<- rbind(textAlign[1:afterIndex,], "left", textAlign[(afterIndex + 1):numRow,])
        fontColor <<- rbind(fontColor[1:afterIndex,], "#000000", fontColor[(afterIndex + 1):numRow,])
        fontSize <<- rbind(fontSize[1:afterIndex,], 11, fontSize[(afterIndex + 1):numRow,])
        fontStyle <<- rbind(fontStyle[1:afterIndex,], "normal", fontStyle[(afterIndex + 1):numRow,])
        padding <<- rbind(padding[1:afterIndex,], 0, padding[(afterIndex + 1):numRow,])
      }
      numRow <<- numRow + 1
    }
  }
)


ConstructFlexTable$methods(
  InsertColumn = function(afterIndex, values) {
    "Insert a column after a given index. Using an index of 0 inserts first."
    if(afterIndex < 1) {
      data <<- cbind(values,data)

      rowSpan <<- cbind(1, rowSpan)
      colSpan <<- cbind(1, colSpan)
      color <<- cbind("#ffffff", color)
      borderWidthVertical <<- cbind(1, borderWidthVertical)
      borderWidthHorizontal <<- cbind(1, borderWidthHorizontal)
      textAlign <<- cbind("left", textAlign)
      fontColor <<- cbind("#000000", fontColor)
      fontSize <<- cbind(11, fontSize)
      fontStyle <<- cbind("normal", fontSize)
      padding <<- cbind(0, padding)
    } else if(afterIndex >= numCol){
      data <<- cbind(data, values)

      rowSpan <<- cbind(rowSpan, 1)
      colSpan <<- cbind(colSpan, 1)
      color <<- cbind(color, "#ffffff")
      borderWidthVertical <<- cbind(borderWidthVertical, 1)
      borderWidthHorizontal <<- cbind(borderWidthHorizontal, 1)
      textAlign <<- cbind(textAlign, "left")
      fontColor <<- cbind(fontColor, "#000000")
      fontSize <<- cbind(fontSize, 11)
      fontStyle <<- cbind(fontSize, "normal")
      padding <<- cbind(padding, 0)
    } else {
      data <<- cbind(data[,1:afterIndex], values, data[,(afterIndex + 1):numCol])

      rowSpan <<- cbind(rowSpan[,1:afterIndex], 1, rowSpan[,(afterIndex + 1):numCol])
      colSpan <<- cbind(colSpan[,1:afterIndex], 1, colSpan[,(afterIndex + 1):numCol])
      color <<- cbind(color[,1:afterIndex], "#ffffff", color[,(afterIndex + 1):numCol])
      borderWidthVertical <<- cbind(borderWidthVertical[,1:afterIndex], 1, borderWidthVertical[,(afterIndex + 1):(numCol + 1)])
      borderWidthHorizontal <<- cbind(borderWidthHorizontal[,1:afterIndex], 1, borderWidthHorizontal[,(afterIndex + 1):(numCol + 1)])
      textAlign <<- cbind(textAlign[,1:afterIndex], "left", textAlign[,(afterIndex + 1):numCol])
      fontColor <<- cbind(fontColor[,1:afterIndex], "#000000", fontColor[,(afterIndex + 1):numCol])
      fontSize <<- cbind(fontSize[,1:afterIndex], 11, fontSize[,(afterIndex + 1):numCol])
      fontStyle <<- cbind(fontStyle[,1:afterIndex], "normal", fontStyle[,(afterIndex + 1):numCol])
      padding <<- cbind(padding[,1:afterIndex], 0, padding[,(afterIndex + 1):numCol])
    }
    numCol <<- numCol + 1
  }
)


ConstructFlexTable$methods(
  InsertHeaderRow = function(afterIndex = 0, values) {
    "Inserts a header row. Makes this row bold by default"
    InsertRow(afterIndex, values)
    i = afterIndex + 1
    isHeaderRow[i] <<- TRUE
    SetFontStyleByRow(i)
  }
)
