#' Round numeric values keeping trailing zeros.
#'
#' \code{roundWZero} rounds the values in its first argument to the specified number of decimal places (default 0).
#'
#'
#' @param x a numeric vector.
#' @param digits integer indicating the number of decimal places
#' @examples
#' roundWZero(c(100,10,1,0.1,0.01,0.001,0.0001),digits = 1)
#' roundWZero(c(100,10,1,0.1,0.01,0.001,0.0001),digits = -1)
#'
#' @importFrom stringr str_split str_length
#' @export roundWZero


roundWZero=function(x,digits=0){

  xstring=paste0(round(x,digits))
  for(j in 1:length(x)){
    if(xstring[j]!="NA"){
      ln=str_length(str_split(xstring[j],"\\.")[[1]][2])
      if(!is.na(ln)){
        if(ln<digits){
          for(i in 1:(digits-ln)){
            xstring[j]=paste0(xstring[j],"0")
          }
        }
      } else {
        if(digits>0){
          xstring[j]=paste0(xstring[j],".")
          for(i in 1:(digits)){
            xstring[j]=paste0(xstring[j],"0")
          }
        }
      }
    }
  }
  return(xstring)

}
