


buildTable=function(input,values){
  if(length(values$var)>0){

    if(!is.null(input$strata)){
      if(input$strata=="None"){
        values$strata=NULL
      } else {
        values$strata=input$strata
      }
    } else {
      values$strata=NULL
    }
    if(!is.null(input$strata_count)){
      values$strata_count=input$strata_count=="TRUE"
    }
    if(!is.null(input$overall)){
      values$overall=input$overall=="TRUE"
    } else {
      values$overall=TRUE
    }
    values$count=input$count
    values$nmax=input$nmax
    values$round_print=ifelse(all(values$round==values$round[1]),values$round[1],values$round)

    # remove missing values
    values$database[values$strata][is.na(values$database[values$strata])]="missing"


    values$table=table_values(values$database,var=values$var,var.names=values$var.names,var.order=values$var.order,type=values$type,strata=values$strata,strata.names=values$strata_names,strata.count=values$strata_count,overall=values$overall,count=values$count,round=values$round,nmax=values$nmax)
  }
  return(values)
}
