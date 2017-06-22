#' table_percent
#'
#' This is a wrapper function for table. It adds percentages to existing counts normally returned by table. This is only written for 1 or 2 variable.
#'
#' @param x first variable to tabulate
#' @param y (optional) second variable to tabulate
#' @param direction "row" or "col" for horizontal vs vertical percentages
#' @param total "both", "row" or "col" for totals columns
#' @param useNA "none","ifany","always" passed directly to table
#' @param rnd round percentages to this many decimal places. This uses \code{\link{roundWZero}} to force zeros to remain in decimals.
#' @export table_percent
table_percent = function(x,y = NULL, direction = "row",total="both", useNA = "ifany", rnd = 1) {

  if(is.null(y)){
    tble2 = table(x,useNA = useNA)
    sm = sum(tble2)
    dmnames = names(tble2)
    tble2 = paste0(tble2, " (", roundWZero(100 * tble2 / sm,rnd), "%)")

    if(total %in% c("both","row","col")){
      tble2 = c(tble2, sm)
      dmnames = c(dmnames, "total")
    }
    names(tble2) = dmnames
    class(tble2) = "table"
  } else {
    tble = table(x,y, useNA = useNA)
    tble2 = tble


    if(direction == "row"){
      sm = rowSums(tble)
      tble2 = t(sapply(1:length(sm), function(x) paste0(tble[x,], " (", roundWZero(100 * tble[x,] / sm[x],rnd), "%)")))
      dimnames(tble2)[[1]] = dimnames(tble)[[1]]
      dimnames(tble2)[[2]] = dimnames(tble)[[2]]
      class(tble2) = "table"
    } else {
      sm = colSums(tble)
      tble2 = sapply(1:length(sm), function(x) paste0(tble[,x], " (", roundWZero(100 * tble[,x] / sm[x],rnd), "%)"))
      dimnames(tble2)[[1]] = dimnames(tble)[[1]]
      dimnames(tble2)[[2]] = dimnames(tble)[[2]]
      class(tble2) = "table"
    }

    if(total %in% c("both","row")) {
      tble2 = cbind(tble2,rowSums(tble))
      tble = cbind(tble,rowSums(tble))
      dimnames(tble2)[[2]][length(dimnames(tble2)[[2]])] = "Total"
    }
    if(total %in% c("both","col")) {
      tble2 = rbind(tble2,colSums(tble))
      dimnames(tble2)[[1]][length(dimnames(tble2)[[1]])] = "Total"
    }
  }

  return(tble2)
}
