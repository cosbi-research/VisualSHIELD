
shinyServer(function(input, output, session) {
  # login information, list of servers and user name
  login <- reactive({
    if( is.null(input$load) || !input$load )
      return(NULL)
    
    isolate( 
      list(username="tomasoni", email="tomasoni@cosbi.eu",
                servers=list(
                  # server 1
                  list(
                          opal_server = list(id = "1",
                                             name = "Server1",
                                             url = input$custom_server_1,
                                             username = "administrator",
                                             password = "password",
                                             certificate = NULL,
                                             private_key = NULL),
                          dashin_server = NULL # dbNP server whose studies will be migrated to the opal server defined above
                  ),
                  # server 2
                  list(
                    opal_server = list(id = "2",
                                       name = "Server2",
                                       url = input$custom_server_2,
                                       username = "administrator",
                                       password = "password",
                                       certificate = NULL,
                                       private_key = NULL),
                    dashin_server = NULL # dbNP server whose studies will be migrated to the opal server defined above
                  )
                  #, ... server n
                )
              ) 
      )
    })
  
  VisualSHIELDServer("VisualSHIELD", servers=login, assume.columns.type="numeric")

  output$server <- renderUI({
    fluidRow(
      column(5, textInput("custom_server_1", label="Server 1:", value="https://localhost:8843", placeholder = "https://opal-demo.obiba.org")),
      column(5, textInput("custom_server_2", label="Server 2:", value="https://localhost:8843", placeholder = "https://opal-demo.obiba.org"))
    )
  })
})
