#' Clean p-values for tabulation and publication
#'
#' This is a small vectorised wrapper to convert pvalues to a significant number of decimal places and replace those less than that with a less than symbol, e.g. to two decimal places 0.007 will return <0.01, but to three it will return 0.007. it also utilises round with zero to keep trailing zeros. This is important for p-values because it shows the accuracy you have reported.
#' @param pvalue A value or vector of numerical pvalues
#' @param digits The number of digits to report to the right of the decimal place
#' @param signif optional parameter. Adds a star to those values less than the value of signif to highlight significant results.
#' @examples
#' pvalues = c(0.1257,0.0675,0.0345,0.0016,0.0009)
#' # Two decimal places
#' prettifyPValue(pvalues, digits=2)
#' # Three decimal places with a star for those less than 0.05
#' prettifyPValue(pvalues, digits=3, signif=0.05)

#' @export prettifyPValue



prettifyPValue=function(pvalue,digits=3, signif=NULL){

  pretty = roundWZero(pvalue,digits)
  pretty[pvalue<10^(-digits)]=paste0("<",10^(-digits))

  if(!is.null(signif)){
    pretty[pvalue<signif]=paste0(pretty[pvalue<signif],"*")
  }
  return(pretty)
}
