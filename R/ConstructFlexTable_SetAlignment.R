ConstructFlexTable$methods(
  SetTextAlign = function(align = "left"){
    "Set the text alignment of all cells. align should be one of 'left', 'right', 'center', 'justify'."
    if(align == "centre"){
      align = "center"
    }
    textAlign[,] <<- align
  }
)

ConstructFlexTable$methods(
  SetTextAlignColByIndex = function(align = "left", index) {
    "Set the text alignment of a specific column or columns. align should be one of 'left', 'right', 'center', 'justify'."
    if(align == "centre"){
      align = "center"
    }
    for(j in index){
      textAlign[,j] <<- align
    }
  }
)

ConstructFlexTable$methods(
  SetTextAlignRowByIndex = function(align = "left", index) {
    "Set the text alignment of a specific row or rows. align should be one of 'left', 'right', 'center', 'justify'."

    if(align == "centre"){
      align = "center"
    }
    for(i in index){
      textAlign[i,] <<- align
    }
  }
)
