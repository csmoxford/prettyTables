#' GetFlexTable
#'
#'
#'
#' Creates a \code{\link{FlexTable}} object using \code{\link{ConstructFlexTable}} objects. Multiple objects can be appended provided they have the same number of rows.
#'
#' This requires ReporteRs to be installed and ReporteRs requires a java JDK to be installed to work correcly.
#'
#'
#' @param ... \code{\link{ConstructFlexTable}} objects
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
    height = height + content[[i]]$numRow
  }


  ft = FlexTable(numrow = height, numcol = content[[1]]$numCol)

  # print(height)
  rowID = 0
  for(ct in content) {
    # print(ct)
    for(i in 1:ct$numRow) {
      if(!ct$isHeaderRow[i]){
        for(j in 1:ct$numCol) {
          if(ct$rowSpan[i,j] > 1) {
            ft = spanFlexTableRows(ft,j,rowID + i,rowID + i+ct$rowSpan[i,j]-1)
          }
          if(ct$colSpan[i,j] > 1) {
            ft = spanFlexTableColumns(ft,i,j,j+ct$colSpan[i,j]-1)
          }
        }
      }
    }
    rowID = rowID + ct$numRow
  }

  rowID = 0
  for(ct in content) {
    # print(ct)
    for(i in 1:ct$numRow) {
      if(!ct$isHeaderRow[i]){
        for(j in 1:ct$numCol) {
          if(ct$colSpan[i,j] > 0 && ct$rowSpan[i,j] > 0) {

            ft[rowID + i,j] <- ct$data[i,j]
            # print(c(i,j,ct$colSpan))
            ft[rowID + i,j] <- parProperties(text.align = ct$textAlign[i,j])

            ft[rowID + i, j] <- textProperties(color = ct$fontColor[i,j],
                                               font.size = ct$fontSize[i,j],
                                               font.weight = ifelse(substr(ct$fontStyle[i,j],1,1)=="b","bold", "normal"),
                                               font.style = ifelse(substr(ct$fontStyle[i,j],1,1)=="i","italic", "normal"))

            ft[rowID + i,j] <- cellProperties(background.color = ct$color[i,j],
                                              border.left.width = ct$borderWidthVertical[i,j],
                                              border.top.width = ct$borderWidthHorizontal[i,j],
                                              border.right.width = ct$borderWidthVertical[i,j + ct$colSpan[i,j]],
                                              border.bottom.width = ct$borderWidthHorizontal[i + ct$rowSpan[i,j],j])
          }
        }
      }
    }
    rowID = rowID + ct$numRow
  }

  # add headerRows if specified in first
  ct = content[[1]]
  i = 1
  while(i <= ct$numRow & ct$isHeaderRow[i]) {
    #print(ct$colSpan[i,] != 0)
    #print(ct$colSpan[i,ct$colSpan[i,] != 0])
    #print(ct$data[i,])
    ft <- addHeaderRow(ft, as.character(ct$data[i,ct$colSpan[i,] != 0]),colspan = ct$colSpan[i,ct$colSpan[i,] != 0])

    for(j in 1:ct$numCol){

      ft[i,j, to = "header"] <- parProperties(text.align = ct$textAlign[i,j])

      ft[i, j, to = "header"] <- textProperties(color = ct$fontColor[i,j],
                                                font.size = ct$fontSize[i,j],
                                                font.weight = ifelse(substr(ct$fontStyle[i,j],1,1)=="b","bold", "normal"),
                                                font.style = ifelse(substr(ct$fontStyle[i,j],1,1)=="i","italic", "normal"))

      ft[i,j, to = "header"] <- cellProperties(background.color = ct$color[i,j],
                                               border.left.width = ct$borderWidthVertical[i,j],
                                               border.top.width = ct$borderWidthHorizontal[i,j],
                                               border.right.width = ct$borderWidthVertical[i,j + ct$colSpan[i,j]],
                                               border.bottom.width = ct$borderWidthHorizontal[i + ct$rowSpan[i,j],j])
    }

    i = i + 1
  }



  return(ft)
}


