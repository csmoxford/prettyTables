#' GetFlexTable
#'
#' Creates a \code{\link{FlexTable}} object using \code{\link{ConstructFlexTable}} objects. Multiple objects can be appended provided they have the same number of rows.
#'
#' This requires ReporteRs to be installed and ReporteRs requires a java JDK to be installed to work correcly.
#'
#'
#' @param ... \code{\link{ConstructFlexTable}} objects
#'
#' Header rows will only be used from the first \code{\link{ConstructFlexTable}} object.
#'
#' @export GetFlexTable
#' @import ReporteRs
GetFlexTable = function(...){
  content = list(...)
  go = TRUE
  for(index in 1:length(content)) {
    ct = content[[index]]
    if(class(ct) != "ConstructFlexTable") {
      message("All inputs should be of class constructFlexTable. Entry (", index, ") was not of class constructFlexTable")
      go = FALSE
    }
  }
  if(!go){
    stop("One or more objects was not of class ConstructFlexTable")
  }

  height = 0
  for(i in 1:length(content)){
    height = height + content[[i]]$numRow - sum(content[[i]]$isHeaderRow)
  }


  ft = FlexTable(numrow = height, numcol = content[[1]]$numCol)

  # print(height)
  rowID = 0
  headCount = 0
  for(ct in content) {
    # print(ct)
    for(i in 1:ct$numRow) {
      if(!ct$isHeaderRow[i]){
        for(j in 1:ct$numCol) {

          if(ct$rowSpan[i,j] > 1) {
            ft = spanFlexTableRows(ft,j,rowID + i - headCount,rowID + i  - headCount + ct$rowSpan[i,j]-1)
          }
          if(ct$colSpan[i,j] > 1) {
            ft = spanFlexTableColumns(ft,rowID + i  - headCount,j,j+ct$colSpan[i,j]-1)
          }
        }
      } else {
        headCount = headCount + 1
      }
    }
    rowID = rowID + ct$numRow
  }


  rowID = 0

  for(ct in content) {
    # print(ct)
    headCount = 0
    for(i in 1:ct$numRow) {
      if(!ct$isHeaderRow[i]){
        for(j in 1:ct$numCol) {
          if(ct$colSpan[i,j] > 0 && ct$rowSpan[i,j] > 0) {
            ft[rowID + i - headCount,j] <- ct$data[i,j]
            # print(c(i,j,ct$colSpan))
            ft[rowID + i - headCount,j] <- parProperties(text.align = ct$textAlign[i,j])

            ft[rowID + i - headCount, j] <- textProperties(
              color = ct$fontColor[i,j],
              font.size = ct$fontSize[i,j],
              font.weight = ifelse(substr(ct$fontStyle[i,j],1,1)=="b","bold", "normal"),
              font.style = ifelse(substr(ct$fontStyle[i,j],1,1)=="i","italic", "normal")
            )

            ft[rowID + i - headCount,j] <- cellProperties(
              background.color = ct$color[i,j],
              border.left.width = ct$borderWidthVertical[i,j],
              border.top.width = ct$borderWidthHorizontal[i,j],
              border.right.width = ct$borderWidthVertical[i,j + ct$colSpan[i,j]],
              border.bottom.width = ct$borderWidthHorizontal[i + ct$rowSpan[i,j],j],
              padding = ct$padding[i,j]
            )
          }
        }
      } else {
        headCount = headCount + 1
      }
    }
    rowID = rowID + ct$numRow - headCount
  }

  # add headerRows if specified in first CFtable object
  ct = content[[1]]
  i = 1
  while(i <= ct$numRow & ct$isHeaderRow[i]) {
    # Note that row span does not work. To ensure correct border are used it is set to 1 for all entries.
    jpos = which(ct$colSpan[i,] != 0)
    ft <- addHeaderRow(ft, as.character(ct$data[i,jpos]),colspan = ct$colSpan[i,jpos])
    rowSpan = ct$rowSpan
    ct$rowSpan[,] = 1
    for(j in jpos){

      ft[i,j, to = "header"] <- parProperties(text.align = ct$textAlign[i,j])

      ft[i, j, to = "header"] <- textProperties(color = ct$fontColor[i,j],
                                                font.size = ct$fontSize[i,j],
                                                font.weight = ifelse(substr(ct$fontStyle[i,j],1,1)=="b","bold", "normal"),
                                                font.style = ifelse(substr(ct$fontStyle[i,j],1,1)=="i","italic", "normal"))

      ft[i,j, to = "header"] <- cellProperties(background.color = ct$color[i,j],
                                               border.left.width = ct$borderWidthVertical[i,j],
                                               border.top.width = ct$borderWidthHorizontal[i,j],
                                               border.right.width = ct$borderWidthVertical[i,j + ct$colSpan[i,j]],
                                               border.bottom.width = ct$borderWidthHorizontal[i + ct$rowSpan[i,j],j],
                                               padding = ct$padding[i,j])
    }
    ct$rowSpan = rowSpan

    i = i + 1
  }



  return(ft)
}


