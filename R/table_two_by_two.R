#' @title Creates tables for baseline characteristics or similar tables
#' @description Provide summary statistics on two or three variables. Rows consist of two variables to summaries and columns may consist of an optional stratification variable. There is also an option to print an overall column alongside the stratified summaries.
#' @param data Dataset to generate table from
#' @param var A character string naming the row variable to stratify by
#' @param var2 A character string naming the variable to summarise over
#' @param var.name Optional. A formatted version of var
#' @param var.name2 Optional. A formatted version of var2
#' @param var.order Optional. A vector of the values of var in the order you want to display them
#' @param var.order2 Optional. A vector of the values of var2 in the order you want to display them. Note that this creates empty rows when there are no values for a given variable.
#' @param type A vector of the method to summarise var2
#' @param strata A string of the column to used to stratify on
#' @param strata.names Otional. A named vector of alternative strata names
#' @param strata.count TRUE/FALSE for displaying strata counts at the top of each column
#' @param overall TRUE/FALSE for including an overall column
#' @param count "n","miss" or "none" providing the counts, missing values or omitting for each column for numeric variables
#' @param round A value or vector for the number of significant figures to report the data to
#' @param messages TRUE/FALSE report messages for missing row names in var.order
#' @details Available methods and values for \strong{Type}:
#' \tabular{cc}{ "miqr" \tab median (Q25,Q75) \cr "miqrr" \tab median (Q25,Q75)[min,max] \cr "mrng" \tab median (Q0,Q100) \cr "avsd" \tab mean (sd) \cr "avci" \tab mean (confidence interval) \cr "st" \tab count \cr "str" \tab count/total \cr "stp" \tab count (percent) \cr "strp" \tab count/total (percent)
#' }
#' @return Returns a data.frame
#' @examples
#' # Data
#' data(iris)
#'
#' # This function does not work with factors so convert to string.
#' iris$Species=as.character(iris$Species)
#'
#' # generate a variable to stratify by since iris only has one target
#' # stratification
#' iris$Petal.split="Short"
#' iris$Petal.split[iris$Petal.Length>4.3]="Long"
#'
#' # build the table
#' table_two_by_two(iris, var="Species",var2="Sepal.Length",
#' var.name2="Sepal Length", type="miqr",strata="Petal.split", round=1)
#'
#'@seealso \code{\link{table_values}}
#'
#' @export table_two_by_two
table_two_by_two = function(data,var, var2, var.name = NULL, var.name2 = NULL, var.order = NULL, var.order2 = NULL, type, strata = NULL, strata.names = NULL, strata.count = TRUE, overall = TRUE, count = "n", round = 3, messages = TRUE){

  #  # Define strata required for table
  all_strata=c()
  if(!is.null(strata)){
    # Add unique strata if required
    if(!is.null(strata.names)){
      all_strata=names(strata.names)
    } else {
      if("" %in% data[[strata]] | sum(is.na(data[[strata]]))>0){
        data[[strata]][data[[strata]] == "" | is.na(data[[strata]])] = "Missing"
      }

      all_strata=unique(as.character(data[[strata]]))
    }
  }

  # Add an overall strata if required
  if(overall){
    # add overall if required
    if(!"Overall" %in% all_strata){
      all_strata=c(all_strata,"Overall")
    }
    if(!is.null(strata.names)){
      if(!"Overall" %in% names(strata.names)){
        strata.names = c(strata.names,"Overall"="Overall")
      }
    }
  }

  if(!is.null(strata.names)){
    if(length(strata.names)!=length(all_strata)){
      warning("If provided, strata.names must include an entry for every strata.")
    }
  }

  # Make round the length of the variables to be summarised if vector not given
  if(length(round) == 1) {
    round = rep(round,length(var))
  }
  if(length(round)!=length(var)) {
    round = rep(round[1], length(var))
    warning("The length of round did not match the length of var using round[1] for all values")
  }
  # If not var names are given then set to the column names
  if(is.null(var.name)) {
    var.name = var
  }
  if(is.null(var.name2)) {
    var.name2 = var2
  }

  # variable ordering if not provided
  if(is.null(var.order)) {
    var.order = sort(unique(data[,var]))
  }

  #
  if(!is.null(var.order2)) {
    if(!(class(var.order2) %in% "list")) {
      var.ord = list()
      var.ord[[var2]] = var.order2
      var.order2 = var.ord
    }
  }

  # preallocate a data.frame:
  tble = matrix("",ncol = length(all_strata) + 2, nrow = 10)
  tble = data.frame(tble, stringsAsFactors = FALSE)
  colnames(tble) = c(" ", "  ", all_strata)
  # For each variable to be summarised switch to desired method
  nxt_row = 1
  i = 1

  for(j in var.order){

    # name row
    tble[nxt_row,1] = j


    # subset the data
    dataSub = data[j == data[,var], ]

    # collect summary data
    dat=switch(
      type,
      miqr = .tbl_miqr(tble, strata, all_strata, dataSub, var2, var.order2, type, nxt_row, round),
      mrng = .tbl_mrng(tble, strata, all_strata, dataSub, var2, var.order2, type, nxt_row, round),
      avsd = .tbl_avsd(tble, strata, all_strata, dataSub, var2, var.order2, type, nxt_row, round),
      avci = .tbl_avci(tble, strata, all_strata, dataSub, var2, var.order2, type, nxt_row, round),
      st   = .tbl_st(tble, strata, all_strata, dataSub, var2, var.order2, type, nxt_row, round, messages),
      str  = .tbl_str(tble, strata, all_strata, dataSub, var2, var.order2, type, nxt_row, round, messages),
      stp  = .tbl_stp(tble, strata, all_strata, dataSub, var2, var.order2, type, nxt_row, round, messages),
      strp = .tbl_strp(tble, strata, all_strata, dataSub, var2, var.order2, type, nxt_row, round, messages)
    )
    # update table and next row
    tble = dat$tble
    nxt_row = dat$nxt_row
    i = i+1
  }

  # add counts for each strata to strata names if required:
  col_names = c()
  if(strata.count) {
    for(j in 1:length(all_strata)) {
      if(all_strata[j] == "Overall"){
        cnt = dim(data)[1]
      } else {
        cnt = sum(data[,strata] == all_strata[j])
      }
      if(!is.null(strata.names)) {
        col_names = c(col_names, paste0(strata.names[all_strata[j]], " (n=", cnt, ")"))
      } else {
        col_names = c(col_names, paste0(all_strata[j], " (n=", cnt, ")"))
      }
    }
  } else {
    col_names = all_strata
  }


  colnames(tble) = c(var.name, var.name2, col_names)

  # Remove unused rows:
  tble = tble[1:(nxt_row-1), ]

  return(tble)
}
