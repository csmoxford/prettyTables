

ConstructFlexTable$methods(
  SetPaddingAll = function(paddingWidth = 0) {
    "Set the padding of all cells. FlexTable itself can accept padding for each side of a cell but I have not provided this funcitonality in this class."
    padding[,] <<- paddingWidth
  }
)
