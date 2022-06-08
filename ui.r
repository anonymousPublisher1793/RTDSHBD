library(shiny)
library(shinythemes)
library(plotly)
library(tippy)
library(shinyBS)


navbarPage( title = 'DASHBOARD',
  tabPanel(
    title = 'OVERVIEW',
    div(id = "myTooltip",
        bsTooltip(id = "metricstable", 
                  title = "F1 Score reflects algorithmic accuracy and considers precision and recall<br><br>Matthews correlation coefficient measures the quality of binary classifications<br><br>False positive rate indicates the amount of algorithmic false positive detections related to all other detections", 
                  placement = "left", 
                  trigger = "hover")),
  theme = shinytheme("flatly"),
  tags$h2("JOHN DOE",
          style="font-size:20px; 
                color:#515A5A; 
                text-align:right;",
          
          img(src= 'path21.png', 
              height = 50, 
              width = 50
              ),
          
          hr(style = "border-top: 1px
                     solid 
                     #D5DBDB;"), 
  fluidRow( 
              column(6,
                     plotlyOutput("plot2",
                                  height = "470px")), 
              column(6,
                     plotlyOutput("plot4",
                           height = "235px"), 
                     plotlyOutput("plot1",
                                  height = "235px"))),
  DT::dataTableOutput("table"),
  fluidRow(
    column (2,
            offset = 5,
            downloadButton('download',"Download data history (.csv)"),
            align = "center", 
            style="margin-top:10px;")))),
 
  tabPanel(
    title = 'SENSORS',
    theme = shinytheme("flatly"),
    tags$h2("JOHN DOE",
            style="font-size:20px; 
                color:#515A5A; 
                text-align:right;",
            
            img(src= 'path21.png', 
                height = 50, 
                width = 50
            ),
            
            hr(style = "border-top: 1px
                     solid 
                     #D5DBDB;")),
    
    fluidRow(
    column(4,
           selectInput("sensors", 
                       h3("Choice of active sensors",
                          style="font-size:16px; 
                          color:#515A5A; 
                          text-align:right;"),
                       choices = list("Sensor 1 of 4" = 1, "Sensor 2 of 4" = 2,
                                      "Sensor 3 of 4" = 3, "Sensor 4 of 4" = 4), selected = 1)),
    column(4,
           selectInput("algorithms", 
                       h3("Choice of anomaly detection algorithms",
                          style="font-size:16px; 
                          color:#515A5A; 
                          text-align:right;"), 
                       choices = list("HTM Java" = 1, "KNN CAD" = 2,
                                      "Relative Entropy" = 3), selected = 1)),
    
    column(4,
           tableOutput("metricstable")),
    fluidRow(
    br(),
    column(6,
           plotlyOutput("barplot",height = "470px")),
    column(6,
           plotlyOutput("barplot2",height = "470px"))),
    br()),
    
    fluidRow(
  
  textOutput("title_occtable"),
  tags$style(HTML("#title_occtable {
                           font-size: 20px; color:#515A5A;
           }")),
  DT::dataTableOutput("occtable"),
  tags$style(HTML(".dataTables_info {
                           display: none;
           }
                  #occtable {
                  font-size: 20px;
                  }")),
  )),
  tabPanel(
    title = 'RULES/CASES',
    theme = shinytheme("flatly"),
    tags$h2("JOHN DOE",
            style="font-size:20px; 
                color:#515A5A; 
                text-align:right;",
            
            img(src= 'path21.png', 
                height = 50, 
                width = 50
            ),
            
            hr(style = "border-top: 1px
                     solid 
                     #D5DBDB;")),
  h2("Applicable rules",
     style = "font-size: 30px;
              color:#515A5A"),
  DT::dataTableOutput("ruletable"),
  tags$style(HTML("#ruletable {
                           font-size: 20px;
           }")),
  actionButton("addrule", "Add comment to rule"),
  tags$style(HTML("#addrule {
                           margin: 10px;
           }")),
  br(),
  h2("Applicable cases",
     style = "font-size: 30px;
              color:#515A5A"),
  tags$style(HTML("#casetable {
                           font-size: 20px;
                           
           }")),
  DT::dataTableOutput("casetable"),
  actionButton("addcase", "Add comment to case"),
  tags$style(HTML("#addcase {
                           margin: 10px;
           }")), 
  h2("Current production configuration",
     style = "font-size: 30px;
              color:#515A5A"),
  tags$style(HTML("#configtable {
                           font-size: 20px;
                           
           }")),
  DT::dataTableOutput("configtable")
),
tags$head(tags$style("
  table.dataTable thead th {
    padding: 8px 10px !important;
  }
"))
)

