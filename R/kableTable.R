#' kableTable
#'
#' This is a wrapper function to allow kable to ouput a table with one input. Either a table or vector can be provided.
#'
#' @param what A table or vector of data to tabulate
#' @param dir Direction to produce table. "h" = horizonatl and anything else will be vertical. Default is "h".
#'
#' @export kableTable
kableTable = function(what, dir="h"){

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
  }



  return(kable(df))
}
