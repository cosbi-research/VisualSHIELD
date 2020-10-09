library(shiny)

library(opalr)
library(DSI)
library(DSOpal)
library(dsBaseClient)
devtools::load_all()

shinyUI(
  fluidPage(
    fluidRow(column(10, uiOutput("server")),
             column(2, actionButton("load", "Update"), tags$style(type='text/css', "#load { vertical-align: middle; margin-top: 25px;}"))),
    fluidRow(VisualSHIELDUI("VisualSHIELD", h3("Demo VisualSHIELD app")))
    )
)
