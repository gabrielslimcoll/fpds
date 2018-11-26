# ================================================================================
# Defense Contracts - Federal Procurement Data System
# Designed and built by Gabriel Coll, Loren Lipsey, and Zhian Wang
# --------------------------------------------------------------------------------
# table app: designed to be flexible for any time-series data
# ================================================================================

# --------------------------------------------------------------------------------
# load packages 

require(shiny)
require(ggplot2)
library(dplyr)
require(scales)
require(Cairo)
require(grid)
require(gridExtra)
library(forcats)
library(shinyBS)
library(DT)
library(reshape2)
library(tidyr)

# --------------------------------------------------------------------------------
# begin ui section 

ui <- 
  
  fluidPage(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Open+Sans');
                    
                    body {
                    font-family: 'Open Sans',  sans-serif;
                    font-weight: 500;
                    line-height: 1.1;
                    color: #554449;
                    }
                    
                    ")),
    
    tags$head(
      tags$style(HTML("body{background-color: #fcfcfc;}"))),
    tags$div(HTML("<div class='fusion-secondary-header'>
                  <div class='fusion-row'>
                  <div class='fusion-alignleft'><div class='fusion-contact-info'><center style=' padding:20px;'><a href='http://csis.org/program/international-security-program' target='_blank'><img class='logo' src='https://defense360.csis.org/wp-content/uploads/2015/08/ISP_new.png' width='40%'></a></center><a href='mailto:'></a></div></div>
                  </div>
                  </div>")),
    tags$style(HTML(".fusion-secondary-header {border-bottom: 3px solid #6F828F}")),
    tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden;}",
                 ".shiny-output-error:before { visibility: hidden;}"
      ),
    tags$style("#error{color: red;
                       font-size: 18px;
                       font-style: bold;}"),
    
    br(),
    fluidRow(
      column(4,
             selectInput("dataset", "Choose a dataset", choices = c("FPDS_Area", "FPDS_Competition", "FPDS_ContractType", "User Select"),
                         selected = "FPDS_Area")),
      column(8,
             conditionalPanel(
               condition = "input.dataset == 'User Select'",
               fileInput('file1', "Please choose a csv file to read, which should have fiscal year in the 1st column and an 'Amount' column.",
                         width = 800,
                         accept=c('text/csv', 'text/comma-separated-values,text/plain'))
             )
      )
    ),
    
    fluidRow(
      column(4,
             radioButtons("view", "View", choices = c("Standard", "Difference"), 
                          selected = "Standard", inline = TRUE)),
      column(4, uiOutput('Checkbox')),
    
      column(4,
        conditionalPanel(
          condition = "input.view == 'Difference'",
          
          shinyjs::useShinyjs(),
          
          uiOutput('Year1'),
          
          uiOutput('Year2'),
          
          radioButtons("zero", "Filter Zero Data", choices = c("Yes", "No"), selected = "No", inline = TRUE),
          bsTooltip("zero", "Filter out programs that have a zero dollar value",
                    placement = "bottom", trigger = "hover",
                    options = NULL)
          
      ))),
    
    
    column(1), align = 'center',
    fluidRow(
      br(), 
      uiOutput("datatable")))

# --------------------------------------------------------------------------------
# begin server section 

options(shiny.maxRequestSize=100*1024^2)

server <- function(input, output, session){
  
  output$Checkbox = renderUI({
    if(input$dataset == 'User Select') {
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      mydata = read.csv(inFile$datapath)
    } else {
      filename <- paste(input$dataset, "_data.csv",sep = "")
      mydata = read.csv(filename, header = TRUE)
    }

    names(mydata)[1] <- "FY"
    checkboxGroupInput('category1', 'Aggregate by',
                       names(mydata)[3:length(names(mydata))-1], selected = names(mydata)[2:6])
    
  })
  
  output$Year1 = renderUI({
    if(input$dataset == 'User Select') {
      inFile <- input$file1
      if (is.null(inFile))
        return(selectInput('year', 'Choose comparison Year', ' '))
      myddata = read.csv(inFile$datapath)
    } else {
      filename <- paste(input$dataset, "_data.csv",sep = "")
      myddata = read.csv(filename, header = TRUE)
    }
    
    names(myddata)[1] <- "FY"
    myddata$FY <-  paste("FY", myddata$FY, sep = "_")
    selectInput("Category1", "Difference between", unique(myddata$FY), 
                selected = unique(myddata$FY)[length(unique(myddata$FY))-1])
    
  })
  
  output$Year2 = renderUI({
    if(input$dataset == 'User Select') {
      inFile <- input$file1
      if (is.null(inFile))
        return(selectInput('year', 'and', ' '))
      myddata = read.csv(inFile$datapath)
    } else {
      filename <- paste(input$dataset, "_data.csv",sep = "")
      myddata = read.csv(filename, header = TRUE)
    }
    
    names(myddata)[1] <- "FY"
    myddata$FY <-  paste("FY", myddata$FY, sep = "_")
    selectInput("Category2", "and", unique(myddata$FY), 
                selected = unique(myddata$FY)[length(unique(myddata$FY))])
    
  })
  
  output$datatable= renderUI({
    if(input$dataset == 'User Select') {
      inFile <- input$file1
      if(is.null(input$file1)){
        h5("Input the data and you can get the detailed information")
      } else {
        mydata = read.csv(inFile$datapath, header = TRUE)
        if("Amount" %in% names(mydata))
          DT::dataTableOutput('tbl')
        else
          htmlOutput("error")
      }
    } else {
      DT::dataTableOutput('tbl')
    }
  })
  
  output$error <- renderUI({
    str1 <- paste('An error occurred. The input file does not have column "Amount".')
    str2 <- paste("Please modified the corresponding column name or choose another file.")
    HTML(paste(str1, str2, sep = '<br/>'))
  }) 

  output$tbl = DT::renderDataTable({
    if(input$dataset == 'User Select') {
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      mydata = read.csv(inFile$datapath, header = TRUE)
    } else {
      filename <- paste(input$dataset, "_data.csv",sep = "")
      
      mydata = read.csv(filename, header = TRUE)

    }
    
    names(mydata)[1] <- "FY"
    
    Dataset <- reactive({
      # standard table
      if(input$view == "Standard"){
        mydata %>%
          group_by_(.dots = c(input$category1, "FY")) %>% 
          summarize(Amount = sum(Amount), na.rm= TRUE)%>%
          mutate(Amount=round(Amount,digits=0))}
      
      # difffernece comparison
      else if(input$view == "Difference"){
        mydata$FY <-  paste("FY", mydata$FY, sep = "_")
        
        mydata <- spread(mydata, FY, Amount)
        
        mutate_call <- lazyeval::interp(~ (x - y), x = as.name(input$Category1), 
                                        y = as.name(input$Category2))
        mutate_call2 <- lazyeval::interp(~ (abs(x - y) / x), x = as.name(input$Category1), 
                                         y = as.name(input$Category2), digits = 2)
        mutate_call3 <- lazyeval::interp(~ (abs(y - x) / y), x = as.name(input$Category1), 
                                         y = as.name(input$Category2))
        mydata %>%
          group_by_(.dots = c(input$category1)) %>% 
          summarize_if(is.numeric,sum,na.rm = TRUE) %>%
          mutate_(.dots = setNames(list(mutate_call), "Difference"))%>%
          mutate_(.dots = setNames(list(mutate_call2), "Percent_Difference"), digits = 2) %>%
          mutate_(.dots = setNames(list(mutate_call3), "Percent_Difference2")) %>%
          mutate(Percent_Difference = pmin(Percent_Difference, Percent_Difference2, na.rm = TRUE))
        
      }
      
    })
    
    category <- reactive({
      if(input$view == "Difference"){
        return(c( input$category1, input$Category1,input$Category2, "Difference", "Percent_Difference"))}
      else{
        return(c( "FY", input$category1, "Amount"))}
      
    })
    
    Dataset2 <- reactive({
      if(input$zero == "Yes"){

        filter_criteria <- lazyeval::interp(~ (x>0 & y >0), x = as.name(input$Category1), 
                                            y = as.name(input$Category2))
        
        filter_(Dataset(), .dots = filter_criteria) }
      else{
        Dataset()
      }})
    
    Dataset_2 <- reactive({mutate(Dataset(), FY = as.factor(FY))})

    if(input$view == "Standard"){
      datatable(
        Dataset_2()[,category()],
        filter = 'top', 
        width = '100%',
        colnames = c("Fiscal Year" = "FY"),
        extensions = 'Buttons',
        options = list("lengthChange" = TRUE,
                       "pageLength" = 50,
                       ordering = TRUE,
                       scrollX = TRUE,
                       scrolly=FALSE,
                       searchHighlight = TRUE,
                       autoWidth = FALSE,
                       dom = 'Bfrtip',
                       buttons = 
                         list('copy', 'print', list(
                           extend = 'collection',
                           buttons = c('csv', 'excel', 'pdf'),
                           text = 'Download'))
        ),
        class = 'cell-border stripe'
      ) %>%
        formatStyle(
          'Amount',
          background = styleColorBar(Dataset()$Amount, '#63c5b8'),
          backgroundSize = '100% 75%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
          
        ) %>% 
        formatCurrency('Amount',currency = "$", interval = 3, mark = ",", digits = 0,
                       dec.mark = getOption("OutDec"))
    }
    else{
      datatable(
        Dataset2()[,category()] %>%
          
          
          na.omit(),
        filter = 'top', 
        width = '100%',
        options = list("lengthChange" = TRUE,
                       "pageLength" = 50,
                       ordering = TRUE,
                       scrollX = TRUE,
                       scrolly=FALSE,
                       searchHighlight = TRUE,
                       autoWidth = FALSE),
        
        
        class = 'cell-border stripe'
      ) %>%

        formatCurrency(c(input$Category1, input$Category2, 'Difference'), currency = "$", interval = 3, mark = ",", digits = 0,
                       dec.mark = getOption("OutDec")) %>%
        formatPercentage('Percent_Difference')
      
      
    }
    
    })
}

# --------------------------------------------------------------------------------
# start app 

shinyApp(ui= ui, server = server)

# ================================================================================