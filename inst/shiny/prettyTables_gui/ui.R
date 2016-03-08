source('Programs/utilities.R')


shinyUI(
  fluidPage(
    theme="Cerulean_bootstrap.css",
    titlePanel("Generate table"),
    fluidRow(
      column(
        3,
        wellPanel(
          tabsetPanel(
            tabPanel(
              "Load Data",
              br(),
              p("Note: Stata and R will not run aditional commands until this window is closed."),
              p("The log file will record the r commands which are used to load the data and create the tables. This can then be used to recreate any tables you have created. The log file records data loading, opening connections to word, any table pushed to word and closing connections to word."),
              textInput("log_file","Set log file",""),
              buttonInput("start_logging",class="btn action-button btn-large btn-success","Start logging"),
              p(),
              textInput("data_path","Path to data (Then select data from list)",""),
              uiOutput("choose_data"),
              buttonInput(id="load_data",class="btn action-button btn-large btn-success",'Load'),
              textOutput("load_tick")
            ),
            tabPanel(
              "Stratify",
              br(),
              p("Note that if there are missing values in the stratification variable these will be replace with missing here but will not carry over to any output code."),
              uiOutput("strata_ui"),
              uiOutput("strata_ui_options")
            ),
            tabPanel(
              "Add/remove Variables",
              br(),
              uiOutput("Variable_submission"),
              uiOutput("Delete_variable"),
              buttonInput(id="build_table2",class="btn action-button btn-large btn-success",'Build Table'),
              ("Build the table to see what it looks like.")
            ),
            tabPanel(
              "Misc",
              selectInput("count","Display column counts for each variable",c("Number of observation"="n","Number of missing observations"="miss","None"="none")),
              numericInput("nmax","Maximum number of rows (increase this if your table has more than 100 rows)",100)
            ),
            tabPanel(
              "Build Table",
              br(),
              p("Build the table to see what it looks like."),
              buttonInput(id="build_table",class="btn action-button btn-large btn-success",'Build Table'),
              p("Output tables to a word document."),
              textInput("rtf_path","Path to document and document name",""),
              buttonInput(id="open_connection",class="btn action-button btn-large btn-warning",'Open RTF connection'),
              p("Tables will only be added if they are pushed using the following button. Multiple tables can be pushed to a single word document."),
              buttonInput(id="push_table",class="btn action-button btn-large btn-success",'Push table to word'),
              p("The connection must be closed in order to save the tables."),
              buttonInput(id="close_connection",class="btn action-button btn-large btn-warning",'Close RTF connection'),
              buttonInput("close_log",class="btn action-button btn-large btn-warning","close log file"),
              p("Close the log if you want to start another. The current log is automatically closed when you close this page")
            )
          )
        )
      ),
      column(
        9,
        tabsetPanel(
          tabPanel(
            "Loaded dataset",
            br(),
            dataTableOutput("database")
            ),
          tabPanel(
            "Tables",
            br(),
            tableOutput("table_variables"),
            tableOutput("table")
          )
        )
      )
    )
  ))
