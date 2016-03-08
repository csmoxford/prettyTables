source('Programs/utilities.R')

shinyServer(function(input, output, session){

  ############################################################################################
  # reset app on close
  session$onSessionEnded(function() {
    try(done(values$rtf))
    try(sink(),silent = TRUE)
    stopApp()
  })



  values=reactiveValues(
    database=NULL,
    var=c(),
    var.name.reset="",
    var.names=c(),
    var.order=c(),
    type=c(),
    strata=NULL,
    strata_names=NULL,
    strata_count=TRUE,
    overall=TRUE,
    count="n",
    round=c(),
    nmax=100,
    table=NULL,
    file.list=c()
  )

  open_log=observe({
    if(!is.null(input$start_logging)){
      if(input$start_logging>0){
        if(!grepl(".",input$log_file,fixed=TRUE)){
          sink(paste0(input$log_file,".R"))
        } else {
          sink(input$log_file)
        }
      }
    }
  })

  close_log=observe({
    if(!is.null(input$close_log)){
      if(input$close_log>0){
        try(sink(),silent = TRUE)
      }
    }
  })

  choose_data=observe({
    if(!is.null(input$data_path)){
      if(input$data_path!=""){
        isolate({
          values$file.list=list.files(path=input$data_path)
          values$file.list=values$file.list[grepl(".csv",values$file.list) | grepl(".txt",values$file.list) | grepl(".dta",values$file.list)]
          output$choose_data=renderUI(selectInput("dataset","Select dataset in folder (.txt or .csv)",choices=values$file.list))
        })
      }
    }
  })

  # Load database:
  load=observe({
    if(!is.null(input$load_data)){
      if(input$load_data>0){
        isolate({
          if(!is.null(input$dataset) & input$data_path!=""){
            if(grepl(".csv",input$dataset) | grepl(".txt",input$dataset)){

              values$var=c()
              values$var.name.reset=""
              values$var.names=c()
              values$var.order=c()
              values$type=c()
              values$strata=NULL
              values$strata_names=NULL
              values$strata_count=TRUE
              values$overall=TRUE
              values$count="n"
              values$round=c()
              values$nmax=100

              values$database=read.csv(paste0(input$data_path,"\\",input$dataset), stringsAsFactors=FALSE)
              output$load_tick=renderText(paste0("Data loaded from: ",input$data_path,"\\",input$dataset))
              # add stata dataset option

              cat("\n###################################################")
              cat("\n# Code created on",format(Sys.time(), "%d%b%Y"))
              cat("\nlibrary(prettyTables)")
              cat("\nlibrary(rtf)")
              cat("\n")
              cat("\n###################################################")
              cat("\n# Directory to data:")
              cat('\nsetwd("',gsub("\\","/",input$data_path,fixed=TRUE),'")',sep="")

              cat("\n###################################################")
              cat("\n# Load data:")
              cat('\ndata=read.csv("',input$dataset,'",',"stringsAsFactors=FALSE)\n",sep = "")

            } else if(grepl(".dta",input$dataset)){
              values$database=read.dta13(paste0(input$data_path,"\\",input$dataset), convert.factors = FALSE)
              output$load_tick=renderText(paste0("Data loaded from: ",input$data_path,"\\",input$dataset))
              # add stata dataset option

              cat("\n###################################################")
              cat("\n# Code created on",format(Sys.time(), "%d%b%Y"))
              cat("\nlibrary(prettyTables)")
              cat("\nlibrary(rtf)")
              cat("\nlibrary(readstata13)")
              cat("\n")
              cat("\n###################################################")
              cat("\n# Directory to data:")
              cat('\nsetwd("',gsub("\\","/",input$data_path,fixed=TRUE),'")',sep="")

              cat("\n###################################################")
              cat("\n# Load data:")
              cat('\ndata=read.dta13("',input$dataset,'",',"convert.factors = FALSE)\n",sep = "")
            } else {
              cat("\n# Missing data path or data file")
            }

            if(!is.null(values$database)){
              output$database=renderDataTable(values$database)
            }
          }
        })
      }
    }
  })

  strata_ui=observe({
    if(!is.null(values$database)){
      isolate({
        output$strata_ui=renderUI({selectInput("strata","Strata",choices=c("None",colnames(values$database)))})
      })
    }
  })

  strata_ui_options=observe({
    if(!is.null(input$strata)){
      if(input$strata!="None"){
        output$strata_ui_options=renderUI({div(
          textInput("strata_names","Strata names (if different to recorded levels)",""),
          p("Example: c(A='Arm A',B='Arm B',Missing='Missing',Overall='Overall')"),
          selectInput("strata_count","Display strata counts",c(TRUE,FALSE)),
          selectInput("overall","Summarise with overall column",c(TRUE,FALSE)))})
      }
    }
  })

  Variable_submission=observe({
    if(!is.null(values$database)){
      values$var.name.reset
      isolate({
        output$Variable_submission=renderUI({div(
          selectInput("variable","Variable",choices=colnames(values$database)),
          textInput("var.name","Variable name (if different to column name)",values$var.name.reset),
          selectInput("type","Display Type",c("miqr - median (Q25,Q75)"="miqr","miqrr - median (Q25,Q75)[min,max]"="miqrr","mrng - median (Q0,Q100)"="mrng","avsd - mean (sd)"="avsd","avci - mean (confidence interval)"="avci","st - count"="st","str - count/total"="str","stp - count (percent)"="stp","strp - count/total (percent)"="strp")),
          numericInput("round","Number of significant figures",3,min=1,step=1),
          buttonInput("submit_variable",name='Submit',class="btn action-button btn-large btn-success"),
          textInput("var.order","Variable order (for currently selected variable)",""),
          buttonInput("submit_order",name='Submit order',class="btn action-button btn-large btn-success")
        )})
      })
    }
  })


  add_varaibles=observe({
    if(!is.null(input$submit_variable)){
      if(input$submit_variable>0){
        isolate({
          values$var=c(values$var,input$variable)
          values$var.names=c(values$var.names,ifelse(input$var.name!="",input$var.name,input$variable))
          values$type=c(values$type,input$type)
          values$round=c(values$round,input$round)
          values$var.name.reset=""
        })
      }
    }
  })

  delete_ui=observe({
    if(length(values$var)>0){
      isolate({
        output$Delete_variable=renderUI({div(
          selectInput("del_row","Delete row number",choices=1:length(values$var)),
          buttonInput("delete_variable",name="Delete",class="btn action-button btn-large btn-warning")
        )})
      })
    }
  })

  delete_variable=observe({
    if(!is.null(input$delete_variable)){
      if(input$delete_variable>0){
        isolate({
          values$var=values$var[-as.numeric(input$del_row)]
          values$var.names=values$var.names[-as.numeric(input$del_row)]
          values$type=values$type[-as.numeric(input$del_row)]
          values$round=values$round[-as.numeric(input$del_row)]
        })
      }
    }
  })

  add_variable_order=observe({
    if(!is.null(input$submit_order)){
      if(input$submit_order>0){
        isolate({
          var.order=eval(parse(text=paste0("c('",gsub(",","','",input$var.order),"')")))
          values$var.order[[input$variable]]=var.order
        })
      }
    }
  })

  build_table=observe({
    if(!is.null(input$build_table)){
      if(input$build_table>0){
        isolate({
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
            if(!is.null(input$strata_names)){
              if(input$strata_names != ""){
                snames=try(eval(parse(text=input$strata_names)),silent=TRUE)
                if(class(snames)=="character"){
                  values$strata_names=snames
                } else {
                  message("\nstrata names could not be parsed to a named vector")
                }
              }
            }

            if(!is.null(input$overall)){
              values$overall=input$overall=="TRUE"
            } else {
              values$overall=TRUE
            }
            values$count=input$count
            values$nmax=input$nmax
            values$round_print=ifelse(all(values$round==values$round[1]),values$round[1],values$round)

            # remove missing values from strata (replacing with missing)
            if(!is.null(values$strata)){
              values$database[values$strata][is.na(values$database[values$strata])]="missing"
              values$database[values$strata][values$database[values$strata]==""]="missing"
            }

            values$table=table_values(values$database,var=values$var,var.names=values$var.names,var.order=values$var.order,type=values$type,strata=values$strata,strata.names=values$strata_names,strata.count=values$strata_count,overall=values$overall,count=values$count,round=values$round,nmax=values$nmax)
          }
        })
      }
    }
  })

  build_table2=observe({
    if(!is.null(input$build_table2)){
      if(input$build_table2>0){
        isolate({
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
            if(!is.null(input$strata_names)){
              if(input$strata_names != ""){
                snames=try(eval(parse(text=input$strata_names)),silent=TRUE)
                if(class(snames)=="character"){
                  values$strata_names=snames
                } else {
                  message("\nstrata names could not be parsed to a named vector")
                }
              }
            }

            if(!is.null(input$overall)){
              values$overall=input$overall=="TRUE"
            } else {
              values$overall=TRUE
            }
            values$count=input$count
            values$nmax=input$nmax
            values$round_print=ifelse(all(values$round==values$round[1]),values$round[1],values$round)

            # remove missing values from strata (replacing with missing)
            if(!is.null(values$strata)){
              values$database[values$strata][is.na(values$database[values$strata])]="missing"
              values$database[values$strata][values$database[values$strata]==""]="missing"
            }

            values$table=table_values(values$database,var=values$var,var.names=values$var.names,var.order=values$var.order,type=values$type,strata=values$strata,strata.names=values$strata_names,strata.count=values$strata_count,overall=values$overall,count=values$count,round=values$round,nmax=values$nmax)
          }
        })
      }
    }
  })


  open_RTF_connection=observe({
    if(!is.null(input$open_connection)){
      if(input$open_connection>0){
        isolate({
          if(is.null(values$rtf)){
            if(!grepl(".",input$rtf_path,fixed=TRUE)){
              rtf_path=paste0(input$rtf_path,".doc")
            } else {
              rtf_path=input$rtf_path
            }
            values$rtf=RTF(file=ifelse(grepl(".doc",rtf_path),input$rtf_path,paste0(input$rtf_path,".doc")), width=8.5, height=11, omi=c(1, 1, 1, 1), font.size=11)
            cat("\n###################################################")
            cat("\n# Open connection to word document:")
            cat('\nrtf=RTF(file="',gsub("\\","/",rtf_path, fixed = T),'", width=8.5, height=11, omi=c(1, 1, 1, 1), font.size=11)\n',sep="")
          }
        })
      }
    }
  })

  push_table=observe({
    if(!is.null(input$push_table)){
      if(input$push_table>0){
        isolate({
          if(length(values$var)>0){
            if(!is.null(values$rtf)){
              ######################################################
              # Build table
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
              if(!is.null(input$strata_names)){
                if(input$strata_names != ""){
                  snames=try(eval(parse(text=input$strata_names)),silent=TRUE)
                  if(class(snames)=="character"){
                    values$strata_names=snames
                  } else {
                    message("\nstrata names could not be parsed to a named vector")
                  }
                }
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


              cat("\n###################################################")
              cat("\n# Add table")
              cat('\naddHeader(rtf,title="")')
              cat('\ntble=table_values(data,var=',deparse(values$var),ifelse(identical(values$var,values$var.names),'',paste0(',var.names=',deparse(values$var.names),collapse = "")),ifelse(deparse(values$var.order)!="list()",paste0(',var.order=',deparse(values$var.order)),''),',type=',deparse(values$type),',strata=',deparse(values$strata),',strata.names=',deparse(values$strata_names),',strata.count=',values$strata_count,',overall=',values$overall,',count=',deparse(values$count),',round=',deparse(values$round_print),ifelse(values$nmax==100,'',paste0(',nmax=',values$nmax)),')',sep="")
              cat('\naddTable(rtf,tble,row.names=FALSE,col.justify="C")')
              cat('\naddParagraph(rtf,title="")')
              cat('\naddNewLine(rtf,n=2)')

              values$table=table_values(values$database,var=values$var,var.names=values$var.names,var.order=values$var.order,type=values$type,strata=values$strata,strata.names=values$strata_names,strata.count=values$strata_count,overall=values$overall,count=values$count,round=values$round,nmax=values$nmax)
              ######################################################

              addTable(values$rtf,values$table,row.names=FALSE,col.justify="C")
              addNewLine(values$rtf,n=2)

            }
          }
        })
      }
    }
  })

  close_connection=observe({
    if(!is.null(input$close_connection)){
      if(input$close_connection>0){
        isolate({
          done(values$rtf)
          values$rtf=NULL
          cat("\n###################################################")
          cat("\n# Close connection to word document")
          cat("\ndone(rtf)")
        })
      }
    }
  })


  output$table_variables=renderTable(data.frame(variable=values$var,variable.name=values$var.names,type=values$type,round=values$round,stringsAsFactors=FALSE))
  output$table=renderTable(values$table,rownames=FALSE)



})
