#' kableTable
#'
#' This is a wrapper function to allow kable to ouput a table with one input. This is not possible for current 1 dimensional tables. Either a table or vector can be provided.
#'
#' @param what A table or vector of data to tabulate
#' @param dir Direction to produce table. "h" = horizonatl and anything else will be vertical. Default is "h".
#' @param verticalColNames A vector with length 2 used as column names if the table is vertical
#'
#' @export kableTable
#' @importFrom knitr kable
kableTable = function(what, dir="h", verticalColNames = c("Name","Count")){

  if(class(what)[1] != "table"){
    what = table(what)
  }


  if(dir=="h"){
    df = data.frame(1)
    for(i in 1:length(what)){
      df[,names(what)[i]] = what[i]
    }
    df$X1 = NULL
  } else {
    df = data.frame(Name=rep("",0), Count=rep(0,0),stringsAsFactors = FALSE)
    for(i in 1:length(what)){
      df[i,] = c(names(what)[i],what[i])
    }
    names(df) <- verticalColNames
  }



  return(kable(df))
}
