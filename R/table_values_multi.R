#' @title A table_values wrapper for multiple choice data
#' @description Takes a dataset and returns a constructed data.frame of counts for multiple choice data collected in a number of columns as text rather than Yes/No or TRUE/FALSE.
#' @param data Dataset to generate table from
#' @param vars A vector of the columns (in string form) to be used to generate the table from
#' @param var.order Optional. A vector of the values to use for the multiple choice. Anything found which does not match the list will be ignored. If messages = TRUE then a message will discuss the differences.
#' @param type A vector of the method to summarise the var
#' @param strata A string of the column to used to stratify on
#' @param strata.names Otional. A named vector of alternative strata names
#' @param strata.count TRUE/FALSE for displaying strata counts at the top of each column
#' @param overall TRUE/FALSE for including an overall column
#' @param count "n","miss" or "none" providing the counts, missing values or omitting for each column for numeric variables
#' @param round A value or vector for the number of significant figures to report the data to
#' @param messages TRUE/FALSE report messages for missing row names in var.order
#' @details
#' Values labeled as "" or NA are ignored
#'
#' Available methods and values for \strong{Type}:
#' \tabular{cc}{ "st" \tab count \cr "str" \tab count/total \cr "stp" \tab count (percent) \cr "strp" \tab count/total (percent)
#' }
#' @details
#' This function essentially itterates over all unique entries in the vars to generate an indicator variable for each one for each row. The grunt of the work is done using \code{var[["option"]] = sapply(1:dim(data)[1], function(i) {max(data[i,vars] == "option", na.rm = TRUE)})}. For more complex tables it may be easier to use this approach first and call table_values with other variables afterwards.
#'
#' Note that column 2 is not removed in making this table so that it can be easily combined with other calls to table_values using rbind. If it is not to be combined this column will be empty so you may want to drop it.
#'
#' @return Returns a data.frame
#' @examples
#' data = data.frame(a=c("Yes","No","No",""),b=c("Yes","Please","No","Yes"), stringsAsFactors = FALSE)
#' vars = c("a","b")
#'
#' table_values_multi(data,vars,type = "st")
#'
#' @export table_values_multi
table_values_multi = function(data, vars, var.order = c(), type, strata = NULL, strata.names = NULL, strata.count = TRUE, overall = TRUE, count = "n", round = 3, messages = TRUE){


  uniques = c()
  for(v in vars){
    uniques = c(uniques, unique(data[,v]))
    uniques = unique(uniques)
  }
  if("" %in% uniques){
    uniques = uniques[-which(uniques == "")]
  }
  if(sum(is.na(uniques))){
    uniques = uniques[-which(is.na(uniques == ""))]
  }

  print(uniques)
  print(var.order)
  print(length(var.order))

  # Get variable ordering
  if(length(var.order) > 0){
    if(!all(uniques %in% var.order)  & messages){
      message("Not all values match values provided. Provided values are: ", paste(sort(var.order),collapse = ", "), " Values found are: ", paste(sort(uniques),collapse = ", "))
    }
  } else {
    var.order = sort(uniques)
  }

  print(var.order)

  for(v in var.order){
    data[[v]] = sapply(1:dim(data)[1], function(i) {max(data[i,vars] == v, na.rm = TRUE)})
    data[,v] = c("No", "Yes")[1+data[,v]]
  }

  var = var.order
  var.name = var.order

  var.order = list()
  for(v in var){
    var.order[[v]] = "Yes"
  }

  print(var.name)
  print(table(data$Yes))


  tb = table_values(data, var, var.name, var.order, type = rep(type, length(var)), strata, strata.names, strata.count, overall, count, round, messages = FALSE)

  tb[,2] = ""
  return(tb)
}
