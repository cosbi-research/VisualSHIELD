
shinyServer(function(input, output, session) {
  # login information, list of servers and user name
  login <- list(username="tomasoni", email="tomasoni@cosbi.eu",
                servers=list(
                  # server 1
                  list(
                          opal_server = list(id = "1",
                                             name = "DEMO",
                                             url = "https://opal-demo.obiba.org",
                                             username = "administrator",
                                             password = "password",
                                             certificate = NULL,
                                             private_key = NULL),
                          dashin_server = NULL # dbNP server whose studies will be migrated to the opal server defined above
                  )
                  #, ... server n
                )
              )

  
  shieldServer <- VisualSHIELDServer("VisualSHIELD", servers=login)
  shieldServer
})