library(shiny)

library(opalr)
library(DSI)
library(DSOpal)
library(dsBaseClient)
devtools::load_all()

shinyUI(
  fluidPage(VisualSHIELDUI("VisualSHIELD", h3("Demo VisualSHIELD app")))
)
