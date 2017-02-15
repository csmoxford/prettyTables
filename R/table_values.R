#' @title Creates tables for baseline characteristics or similar tables
#' @description Takes a dataset and returns a constructed data.frame summarising the variables of interest. This may be subset by strata and display an overall summary. A shiny user interface is provided (\code{\link{shiny_table_values}}). This runs through creating a table and prints r code to the console or a log file to rerun with an rtf wrapper to export directly to word.
#' @param data Dataset to generate table from
#' @param var A vector of the columns (in string form) to be used to generate the table from
#' @param var.names Optional. An alternative (cleaned) list of variable names to use in the table. These may contain spaces
#' @param var.order Optional. A list containing vectors of strings named using the column names in var. This is used to reorder count data items if the original ordering is not acceptable
#' @param type A vector of the method to summarise the var
#' @param strata A string of the column to used to stratify on
#' @param strata.names Otional. A named vector of alternative strata names
#' @param strata.count TRUE/FALSE for displaying strata counts at the top of each column
#' @param overall TRUE/FALSE for including an overall column
#' @param count "n","miss" or "none" providing the counts, missing values or omitting for each column for numeric variables
#' @param round A value or vector for the number of significant figures to report the data to
#' @param messages TRUE/FALSE report messages for missing row names in var.order
#' @param zeros TRUE/FALSE whether to print zeros if none are found defaults to FALSE.
#' @details
#' Values labeled as "" or NA are treated as missing values and are thus listed as such. Values labeled as NaN or "NaN" are treated as not applicable and thus are ignored.
#'
#' Available methods and values for \strong{Type}:
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
#' # build the table (without strata)
#' table_values(iris, var=c("Species","Sepal.Length","Sepal.Width","Petal.Length",
#' "Petal.Width"),var.names=NULL, type=c("str","miqr","miqrr","miqrr","miqrr"), round=1)
#'
#' # build the table (with strata)
#' table_values(iris, var=c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"),
#' var.names=c("Sepal Length","Sepal Width","Petal Length","Petal Width"),
#' strata="Species", type=c("miqr","miqr","miqr","miqr"), round=1)
#'
#' @seealso \code{\link{table_two_by_two}}
#'
#' @export table_values
table_values = function(data, var, var.names = NULL, var.order = list(), type, strata = NULL, strata.names = NULL, strata.count = TRUE, overall = TRUE, count = "n", round = 3, messages = TRUE, zeros = FALSE){

  # Define strata required for table
  all_strata=c()
  if(!is.null(strata)){
    # Add unique strata if required
    if(!is.null(strata.names)){
      all_strata = names(strata.names)
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
  if(length(round) == 1){
    round=rep(round,length(var))
  }
  if(length(round) != length(var)){
    round = rep(round[1],length(var))
    warning("The length of round did not match the length of var using round[1] for all values")
  }
  # If not var names are given then set to the column names
  if(is.null(var.names)){
    var.names = var
  }
  if(length(var.names) != length(var)){
    stop("The length of var and var.names must be equal")
  }

  # preallocate a data.frame:
  tble = matrix("",ncol=length(all_strata)+2,nrow=10)
  tble = data.frame(tble,stringsAsFactors=FALSE)
  colnames(tble) = c(" ","  ",all_strata)
  # For each variable to be summarised switch to desired method

  nxt_row = 1
  for(i in 1:length(var)){
    tt = 0
    if(tt == 1){
    if(!is.null(var.order[[var[i]]])){
      var.order[[var[i]]] = unique(c(var.order[[var[i]]],data[[var[i]]]))
    } else {
      var.order[[var[i]]] = unique(data[[var[i]]])
    }
    if("NaN" %in% var.order[[var[i]]]){
      var.order[[var[i]]] = var.order[[var[i]]][-which(var.order[[var[i]]]=="NaN")]
    }
    if(sum(is.nan(var.order[[var[i]]]))>0){
      var.order[[var[i]]] = var.order[[var[i]]][-which(is.nan(var.order[[var[i]]]))]
    }
    }

    # name row
    tble[nxt_row, 1] = var.names[i]

    # collect summary data
    dat = switch(
      type[i],
      miqr = .tbl_miqr(tble,strata,all_strata,data,var[i],var.order,type[i],count,nxt_row,round[i]),
      miqrr= .tbl_miqrr(tble,strata,all_strata,data,var[i],var.order,type[i],count,nxt_row,round[i]),
      mrng = .tbl_mrng(tble,strata,all_strata,data,var[i],var.order,type[i],count,nxt_row,round[i]),
      avsd = .tbl_avsd(tble,strata,all_strata,data,var[i],var.order,type[i],count,nxt_row,round[i]),
      avci = .tbl_avci(tble,strata,all_strata,data,var[i],var.order,type[i],count,nxt_row,round[i]),
      st   = .tbl_st(tble,strata,all_strata,data,var[i],var.order,type[i],nxt_row,round[i], messages, zeros),
      str  = .tbl_str(tble,strata,all_strata,data,var[i],var.order,type[i],nxt_row,round[i], messages, zeros),
      stp  = .tbl_stp(tble,strata,all_strata,data,var[i],var.order,type[i],nxt_row,round[i], messages, zeros),
      strp = .tbl_strp(tble,strata,all_strata,data,var[i],var.order,type[i],nxt_row,round[i], messages, zeros)
      )

    # update table and next row
    tble = dat$tble
    nxt_row = dat$nxt_row
  }

  # add counts for each strata to strata names if required:
  col_names=c()
  if(strata.count){
    for(j in 1:length(all_strata)){
      if(all_strata[j]=="Overall"){
        cnt = dim(data)[1]
      } else {
        cnt = sum(data[,strata]==all_strata[j])
      }
      if(!is.null(strata.names)){
        col_names = c(col_names,paste0(strata.names[all_strata[j]]," (n=",cnt,")"))
      } else {
        col_names = c(col_names,paste0(all_strata[j]," (n=",cnt,")"))
      }
    }
  } else {
    if(!is.null(strata.names)){
    col_names = strata.names[all_strata]
    } else {
      col_names = all_strata
    }
  }

  colnames(tble) = c(" ", "  ", col_names)

  # Remove unused rows:
  tble = tble[1:(nxt_row-1), ]

  tble[is.na(tble[,1]), 1] = ""

  return(tble)
}


