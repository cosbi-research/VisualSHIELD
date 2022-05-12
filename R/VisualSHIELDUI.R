#' Load main VisualSHIELD UI component
#' this function generates the UI for the shiny module corresponding to this app.
#'
#' @param id The id of the module. It should match with the id param of the VisualSHIELDServer function
#' @param title The title of the module (as a shiny UI object such as h4) in your custom app.
#' @export
#' @examples
#' \dontrun{
#' library(shiny)
#' library(opalr)
#' library(DSI)
#' library(dsBaseClient)
#' library(VisualSHIELD)
#'
#' shinyUI(
#'  fluidPage(
#'    fluidRow(column(10, uiOutput("server")),
#'             column(2, actionButton("load", "Update"), 
#'             tags$style(type='text/css', 
#'         "#load { vertical-align: middle; margin-top: 25px;}"))),
#'    fluidRow(VisualSHIELDUI("VisualSHIELD", h3("Demo VisualSHIELD app")))
#'    )
#')
#'}


VisualSHIELDUI <- function(id, title){
#  library(shinydashboard)
#  library(shinyjs)
  ns <- shiny::NS(id)
  
  shinydashboard::dashboardPage(
    tags$head(tags$style(HTML('.selectize-input {max-height: 200px; overflow-y: visible;}'))),
    # Application title
    #titlePanel(title = "", windowTitle = "DASH-IN interactive federated analysis system"),
    shinydashboard::dashboardHeader(title = title),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Data Servers", tabName = ns("datasources"), icon = shiny::icon("server")),
        shinydashboard::menuItem("Analysis", icon = shiny::icon("bar-chart"), tabName = ns("analysis"))
        #badgeLabel = "new", badgeColor = "green")
      )
    ),  
    shinydashboard::dashboardBody(
      shiny::tags$head(
        shiny::tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
      ),
      shinyjs::useShinyjs(),
      shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = ns("datasources"),
                                shiny::fluidRow(
                                  shiny::fluidRow(
                                    shinydashboard::box(width = 6, title = "Instructions", status = "primary", solidHeader = TRUE,
                                                        shiny::tagList(shiny::h4("(1) Select the desired DBNP / OPAL studies."),
                                                                       shiny::h4("(2) Press the 'Setup remote data tables' button."))
                                    ),
                                    shinydashboard::box(width = 6, title = "Load data tables", status = "primary", solidHeader = TRUE,
                                                        shiny::uiOutput(ns("dsActions")),
                                                        # this will contain an output label
                                                        shiny::uiOutput(ns("dsProjectSingleTableLoadStatus")),
                                                        # this will contain an output label
                                                        shiny::uiOutput(ns("dsProjectTableLoadStatus"))
                                    )
                                  ),
                                  shiny::uiOutput(ns("serversList"))
                                )
        ),
        shinydashboard::tabItem(tabName = ns("analysis"),
                                shiny::conditionalPanel(condition=paste0("!output['", ns('analysisReady'),"']"),
                                                        shiny::fluidRow(shinydashboard::box(width=12,
                                                                            shiny::h4('Select the data you want to analyse from the "Data Servers" menu')
                                                        ))
                                ),
                                shiny::conditionalPanel(condition=paste0("output['",ns('analysisReady'),"']"),
                                                        shiny::fluidRow(
                                                          shinydashboard::box(width=12,
                                                                              title = "General data source Informations", status = "primary", solidHeader = TRUE,
                                                                              collapsible = TRUE,
                                                                              shiny::uiOutput(outputId = ns("generalInfos"))
                                                          )
                                                        ),
                                                        shiny::fluidRow(
                                                          # ------------------------------ MAIN PANEL ---------------------------------------
                                                          shinydashboard::box(id =ns("analysisResultsBox"), width=8,
                                                                              title = "Analysis results", status = "primary", solidHeader = TRUE,
                                                                              collapsible = TRUE,
                                                                              
                                                                              # plot for explorative analysis
                                                                              shiny::conditionalPanel(
                                                                                condition = paste0("input['",ns('plotType'),"'] == 'hist' || input['",ns('plotType'),"'] == 'contour' || input['",ns('plotType'),"'] == 'heatmap' || input['",ns('plotType'),"'] == 'boxplot' || input['",ns('plotType'),"'] == 'correlation' || input['",ns('plotType'),"'] == 'princomp' || input['",ns('plotType'),"'] == 'randomforest' || input['",ns('plotType'),"'] == 'vim'"),
                                                                                
                                                                                shiny::uiOutput(outputId = ns("plotStatus")),
                                                                                shiny::plotOutput(ns("distPlot")),
                                                                                shiny::uiOutput(outputId = ns("plotDownload"))
                                                                              ),
                                                                              # plot for       
                                                                              shiny::conditionalPanel(
                                                                                condition = paste0("input['",ns('plotType'),"'] == 'analisys'"),
                                                                                shiny::uiOutput(outputId = ns("analysisStatus")),
                                                                                shiny::uiOutput(outputId = ns("modelFormula")),
                                                                                shiny::uiOutput(outputId = ns("modelCoefficients")),
                                                                                shiny::uiOutput(outputId = ns("modelSummary")),
                                                                                shiny::uiOutput(outputId = ns("modelDownload"))
                                                                              )
                                                                              
                                                                              
                                                          ),
                                                          # Sidebar with configuration controls
                                                          shinydashboard::box(id =ns("analysisSidebarBox"), width=4,
                                                                              # this will contain the selectInput with id 'plotType'
                                                                              shiny::uiOutput(ns("plotType")),
                                                                              
                                                                              # if data exploration
                                                                              shiny::conditionalPanel(
                                                                                condition = paste0("input['",ns('plotType'),"'] != 'analisys'"),
                                                                                
                                                                                # this will contain the var_x and var_y selectInput
                                                                                shiny::uiOutput(ns("varXY"))
                                                                              ),
                                                                              # if analysis
                                                                              shiny::conditionalPanel(
                                                                                condition = paste0("input['",ns('plotType'),"'] == 'analisys'"),
                                                                                
                                                                                # this will contain the var_explanatory and var_target selectInput
                                                                                shiny::uiOutput(ns("varAnalysis"))
                                                                                # end if analysis
                                                                              )
                                                          )
                                                          # close fluidRow
                                                        ),
                                                        shiny::conditionalPanel(
                                                          condition = paste0("input['",ns('plotType'),"'] == 'analisys'"),
                                                          shiny::fluidRow(
                                                            shinydashboard::box(id =ns("helpBox"), width=12,
                                                                                title = "Help", status = "primary", solidHeader = TRUE,
                                                                                collapsible = TRUE,
                                                                                shiny::h4("Dependent variable:"),
                                                                                shiny::helpText("The variable we would like to predict using the explanatory variables. If the selected dependent variable is categorical and the output distribution is continuous (as in linear regression or GLM+Poisson case) the variable will be automatically converted to numeric."),
                                                                                shiny::h4("Explanatory variables:"),
                                                                                shiny::helpText("They are variable names we want to test for association with the dependant one. More than one explainatory variables can be separated by the '+' operator. The interaction between two variables can be tested as well separating those with ':' operator. The '*' operator denotes factor crossing thefore 'a*b' is interpreted as 'a + b + a:b'. That means the following three associations will be tested: variable 'a',  variable 'b' and the interaction of variables 'a and b'."),
                                                                                shiny::helpText("It is possible to change the variable types by postponing '#num' or '#fac' to the variable name, e.g. 'cat_var#num'."),
                                                                                shiny::h4("Null Explanatory variables:"),
                                                                                shiny::helpText("The null model that will be used to perform the F and R squared tests against the full model. By default the constant model '1' is proposed (only interceipt)")
                                                            )
                                                          )
                                                        )
                                                        #conditionalPanel(condition="output.analysisReady",
                                )
        )
        # close tabItems
      )
      # close dashboardBody
    )
  )
}
  
