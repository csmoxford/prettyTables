% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_values_multi.R
\name{table_values_multi}
\alias{table_values_multi}
\title{A table_values wrapper for multiple choice data}
\usage{
table_values_multi(data, vars, var.order = c(), type, strata = NULL,
  strata.names = NULL, strata.count = TRUE, overall = TRUE, count = "n",
  round = 3, messages = TRUE)
}
\arguments{
\item{data}{Dataset to generate table from}

\item{vars}{A vector of the columns (in string form) to be used to generate the table from}

\item{var.order}{Optional. A vector of the values to use for the multiple choice. Anything found which does not match the list will be ignored. If messages = TRUE then a message will discuss the differences.}

\item{type}{A vector of the method to summarise the var}

\item{strata}{A string of the column to used to stratify on}

\item{strata.names}{Otional. A named vector of alternative strata names}

\item{strata.count}{TRUE/FALSE for displaying strata counts at the top of each column}

\item{overall}{TRUE/FALSE for including an overall column}

\item{count}{"n","miss" or "none" providing the counts, missing values or omitting for each column for numeric variables}

\item{round}{A value or vector for the number of significant figures to report the data to}

\item{messages}{TRUE/FALSE report messages for missing row names in var.order}

\item{zeros}{TRUE/FALSE report zeros (TRUE) or leave cell blank?}
}
\value{
Returns a data.frame
}
\description{
Takes a dataset and returns a constructed data.frame of counts for multiple choice data collected in a number of columns as text rather than Yes/No or TRUE/FALSE.
}
\details{
Values labeled as "" or NA are ignored

Available methods and values for \strong{Type}:
\tabular{cc}{ "st" \tab count \cr "str" \tab count/total \cr "stp" \tab count (percent) \cr "strp" \tab count/total (percent)
}

This function essentially itterates over all unique entries in the vars to generate an indicator variable for each one for each row. The grunt of the work is done using \code{var[["option"]] = sapply(1:dim(data)[1], function(i) {max(data[i,vars] == "option", na.rm = TRUE)})}. For more complex tables it may be easier to use this approach first and call table_values with other variables afterwards.

Note that column 2 is not removed in making this table so that it can be easily combined with other calls to table_values using rbind. If it is not to be combined this column will be empty so you may want to drop it.
}
\examples{
data = data.frame(a=c("Yes","No","No",""),b=c("Yes","Please","No","Yes"), stringsAsFactors = FALSE)
vars = c("a","b")

table_values_multi(data,vars,type = "st")

}
