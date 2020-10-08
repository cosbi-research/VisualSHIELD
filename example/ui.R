library(shiny)
devtools::load_all()

shinyUI(
  fluidPage(VisualSHIELDUI("VisualSHIELD", h3("Demo VisualSHIELD app")))
)
