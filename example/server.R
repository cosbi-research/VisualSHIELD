
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
                                             name = "DEMO",
                                             url = input$custom_server,
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
  
  VisualSHIELDServer("VisualSHIELD", servers=login)

  output$server <- renderUI({
    textInput("custom_server", label="Server to connect to:", value="", placeholder = "https://opal-demo.obiba.org")
  })
})
