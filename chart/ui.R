# ================================================================================
# Defense Contracts - Federal Procurement Data System
# Designed and built by Gabriel Coll, Loren Lipsey, and Zhian Wang
# --------------------------------------------------------------------------------
# chart app: designed to be flexible for any time-series data
# ================================================================================

# --------------------------------------------------------------------------------
# load packages

library(shiny)
library(shinyjs)
library(shinyBS)
library(tidyverse)

# --------------------------------------------------------------------------------
# create categories

breakout <- c(
  "Category",
  "Customer",
  "Contract_Type",
  "Competition",
  "Platform_Portfolio",
  "Vendor_Size"
)

category <- c("Products",
              "Services",
              "R&D")

vendorsize <- c("Big Five",
                "Large",
                "Medium",
                "Small")

customer <- c("Army",
              "Navy",
              "Air Force",
              "MDA",
              "DLA",
              "Other DoD")

portfolio <- c(
  "Aircraft and Drones",
  "Electronics and Communications",
  "Facilities and Construction",
  "Land Vehicles",
  "Missile and Space Systems",
  "Ships & Submarines",
  "Weapons and Ammunition",
  "Other Products",
  "Other R&D and Knowledge Based",
  "Other Services"
)

contract.type <- c(
  "Combination",
  "Cost Reimbursement",
  "Fixed Price",
  "Time and Materials",
  "Other",
  "Unlabeled"
)

classification <- c(
  "Competition with single offer",
  "Effective Competition",
  "No competition",
  "Unlabeled"
)

# --------------------------------------------------------------------------------
# begin ui section
shinyUI <- fluidPage(
  useShinyjs(),
  
  # --------------------------------------------------------------------------------
  # import font
  
  tags$style(
    HTML(
      "
      @import url('//fonts.googleapis.com/css?family=Open+Sans');
      
      body {
      font-family: 'Open Sans',  sans-serif;
      font-weight: 500;
      line-height: 1.1;
      color: #554449;
      }
      
      "
    )
    ),
  
  # --------------------------------------------------------------------------------
  # design theme
  
  tags$head(tags$style(
    HTML("body{background-color: #fcfcfc;}")
  )),
  tags$div(
    HTML(
      "<div class='fusion-secondary-header'>
      <div class='fusion-row'>
      <div class='fusion-alignleft'><div class='fusion-contact-info'><center style=' padding:20px;'><a href='http://csis.org/program/international-security-program' target='_blank'><img class='logo' src='https://defense360.csis.org/wp-content/uploads/2015/08/ISP_new.png' width='40%'></a></center><a href='mailto:'></a></div></div>
      </div>
      </div>"
    )
    ),
  tags$style(
    HTML(".fusion-secondary-header {border-bottom: 3px solid #6F828F}")
  ),
  br(),
  tags$style(HTML(".irs-bar {background: #63c5b8}")),
  tags$style(HTML(".irs-bar {border-top: 1px #63c5b8}")),
  tags$style(HTML(".irs-bar {border-bottom: 1px #63c5b8}")),
  tags$style(HTML(
    ".irs-single, .irs-to, .irs-from {background: #628582}"
  )),
  tags$style(HTML(".irs-max {color: #554449}")),
  tags$style(HTML(".irs-min {color: #554449}")),
  tags$style(HTML(".irs-bar-edge {border: 1px #63c5b8}")),
  tags$style(HTML(
    ".irs-bar-edge {border-color: 1px #63c5b8}"
  )),
  tags$style(HTML(
    ".irs-bar-edge {border-color: 1px #63c5b8}"
  )),
  
  # --------------------------------------------------------------------------------
  # start new row
  
  fluidRow(
    # left column - column sizes should add up to 12, this one is 3 so
    # the other one will be 9
    column(
      3,
      align = 'center',
      br(),
      id = "selectinput",
      #choose Breakout variable
      selectInput(
        inputId = "color_var",
        label = "Breakout",
        choices = breakout,
        selected = "Category",
        width = '100%',
        selectize = TRUE
      ),
      
      # year slider
      sliderInput(
        'Yr',
        "Year Range:",
        min = 2000,
        max = 2016,
        value = c(2000, 2016),
        ticks = FALSE,
        step = 1,
        width = '100%',
        sep = ""
      ),
      # area
      conditionalPanel(
        condition = "input.color_var != 'Category'",
        bsButton(
          inputId = "CatButton",
          label = strong("Area"),
          style = "default",
          value = 0,
          type = "toggle",
          size = "medium",
          block = TRUE
        ),
        
        conditionalPanel(
          condition = "input.CatButton == 1",
          selectInput(
            "Cat",
            label = NULL,
            category,
            multiple = TRUE,
            selectize = FALSE,
            selected = category,
            width = '100%'
          )
        )
      ),
      
      # customer
      conditionalPanel(
        condition = "input.color_var != 'Customer'",
        bsButton(
          inputId = "CustomerButton",
          label = strong("Customer"),
          style = "default",
          value = 0,
          type = "toggle",
          size = "medium",
          block = TRUE
        ),
        
        conditionalPanel(
          condition = "input.CustomerButton == 1",
          selectInput(
            "Customer",
            label = NULL,
            customer,
            multiple = TRUE,
            selectize = FALSE,
            selected = customer,
            width = '100%'
          )
        )
      ),
      
      # contract type
      conditionalPanel(
        condition = "input.color_var != 'Contract_Type'",
        bsButton(
          inputId = "ContractButton",
          label = strong("Contract Type"),
          style = "default",
          value = 0,
          type = "toggle",
          size = "medium",
          block = TRUE
        ),
        
        conditionalPanel(
          condition = "input.ContractButton == 1",
          selectInput(
            "Contract",
            label = NULL,
            contract.type,
            multiple = TRUE,
            selectize = FALSE,
            selected = contract.type,
            width = '100%'
          )
        )
      ),
      # comnpetition
      conditionalPanel(
        condition = "input.color_var != 'Competition'",
        bsButton(
          inputId = "ClassButton",
          label = strong("Competition"),
          style = "default",
          value = 0,
          type = "toggle",
          size = "medium",
          block = TRUE
        ),
        
        conditionalPanel(
          condition = "input.ClassButton == 1",
          selectInput(
            "Classification",
            label = NULL,
            classification,
            multiple = TRUE,
            selectize = FALSE,
            selected = classification,
            width = '100%'
          )
        )
      ),
      
      # platform
      conditionalPanel(
        condition = "input.color_var != 'Platform_Portfolio'",
        bsButton(
          inputId = "PortfolioButton",
          label = strong("Platform Portfolio"),
          style = "default",
          value = 0,
          type = "toggle",
          size = "medium",
          block = TRUE
        ),
        
        conditionalPanel(
          condition = "input.PortfolioButton == 1",
          selectInput(
            "Portfolio",
            label = NULL,
            portfolio,
            multiple = TRUE,
            selectize = FALSE,
            selected = portfolio,
            width = '100%'
          )
        )
      ),
      
      # vendor size
      conditionalPanel(
        condition = "input.color_var != 'Vendor_Size'",
        bsButton(
          inputId = "VSButton",
          label = strong("Vendor Size"),
          style = "default",
          value = 0,
          type = "toggle",
          size = "medium",
          block = TRUE
        ),
        
        conditionalPanel(
          condition = "input.VSButton == 1",
          selectInput(
            "VS",
            label = NULL,
            vendorsize,
            multiple = TRUE,
            selectize = FALSE,
            selected = vendorsize,
            width = '100%'
          )
        )
      ),
      
      br(),
      tags$head(tags$style(
        HTML('#reset_input{background-color:#e3e5ea}')
      )),
      
      bsButton(
        inputId = "reset_input",
        label = strong("Reset"),
        style = "default",
        size = "default",
        width = '100%',
        block = TRUE
      ),
      
      br(),
      radioButtons(
        "Chart",
        "Chart",
        c("Line", "Bar"),
        inline = TRUE,
        selected = "Line"
      ),
      
      br(),
      downloadLink('CSVDownloadBtn',
                   "Download Displayed Data (csv)")
    ),
    
    # left column - column sizes should add up to 12, this one is 9 so
    # the other one will be 3
    column(
      9,
      align = "center",
      div(style = "position:relative",
          uiOutput("PlotPart"),
          
          uiOutput("hover_info"))
    )
  )
  
  # end ui section
    )

# ================================================================================
