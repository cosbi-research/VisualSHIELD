#' Load main VisualSHIELD Server component
#' this function generates the Server counterpart for the UI with the matching id parameter.
#' 
#' @param id The id of the corresponding VisualSHIELD UI.
#' @param servers A R list with user information (name/email)and a list of opal/dbNP servers the user can connect to.
#' @param LOG_FILE The path to the file where user activity will be logged
#' @param glm_max_iterations The maximum number of iterations allowed for the federated analysis
#' @export
#' @examples
#' \dontrun{
#' shinyServer(function(input, output, session) {
#'  # login information, list of servers and user name
#'  login <- reactive({
#'    if( is.null(input$load) || !input$load )
#'      return(NULL)
#'
#'    isolate(
#'      list(username="tomasoni", email="tomasoni@cosbi.eu",
#'                servers=list(
#'                  # server 1
#'                  list(
#'                          opal_server = list(id = "1",
#'                                             name = "DEMO",
#'                                             url = input$custom_server,
#'                                             username = "administrator",
#'                                             password = "password",
#'                                             certificate = NULL,
#'                                             private_key = NULL),
#'                          # dbNP server whose studies will be migrated 
#'                          # to the opal server defined above
#'                          dashin_server = NULL 
#'                  )
#'                  #, ... server n
#'                )
#'              )
#'      )
#'    })
#'
#'  VisualSHIELDServer("VisualSHIELD", servers=login)
#'
#'   output$server <- renderUI({
#'     textInput("custom_server", 
#'               label="Server to connect to:", value="", 
#'               placeholder = "https://opal-demo.obiba.org")
#'   })
#' })
#' }

VisualSHIELDServer <- function(id, servers, LOG_FILE="VisualSHIELD.log", glm_max_iterations=30){
  
#  library(jsonlite)
#  library(httr)
#  library(PhenotypeDatabaseRClient)
#  library(DT)
  
#  library(ggpubr)
#  library(cowplot)
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      extraComboBoxes <- list()
      globalValues <- shiny::reactiveValues()
      available_glm_models = list("identity","logistic","log")
      names(available_glm_models) <- c("gaussian","binomial","poisson")
      
      # === URL PARSING AND DATABASE CONNECTION
      load.data.servers <- function(jdata){
        #> str(jdata)
        #List of 3
        #$ username: chr "dtomasoni"
        #$ email   : chr "danilo.tomasoni@cryptolab.net"
        #$ servers :List of 2
        #..$ :List of 2
        #.. ..$ opal_server  :List of 7
        #.. .. ..$ id         : chr "153"
        #.. .. ..$ name       : chr "OpalRecas BARI"
        #.. .. ..$ url        : chr "http://opal.cloud.ba.infn.it:8080"
        #.. .. ..$ username   : chr "rosario.lombardo"
        #.. .. ..$ password   : chr "XXXX"
        #.. .. ..$ certificate: NULL
        #.. .. ..$ private_key: NULL
        #.. ..$ dashin_server: NULL
        #..$ :List of 2
        #.. ..$ opal_server  :List of 7
        #.. .. ..$ id         : chr "154"
        #.. .. ..$ name       : chr "TNO"
        #.. .. ..$ url        : chr "http://msb1.hex.tno.nl"
        #.. .. ..$ username   : chr "administrator"
        #.. .. ..$ password   : chr "XXXX!"
        #.. .. ..$ certificate: NULL
        #.. .. ..$ private_key: NULL
        #.. ..$ dashin_server:List of 4
        #.. .. ..$ url     : chr "https://dashin.eu/interventionstudies-test/api/"
        #.. .. ..$ username: chr "zefoteus"
        #.. .. ..$ password: chr "XXXXXXXX"
        #.. .. ..$ skey    : chr "XXXXX"
        
        globalValues$username <- username <- jdata$username
        opalServers <- lapply(jdata$servers, function(server){ server$opal_server })
        dbnpServers <- lapply(jdata$servers, function(server){ if(is.null(server$dashin_server)) return(NULL); s <- server$dashin_server; s$id <- server$opal_server$id; s })
        dbnpServers <- dbnpServers[!vapply(dbnpServers, is.null, logical(1))]
        ############## SETTING DBNP SERVERS ##########################
        if(length(dbnpServers) > 0){
          df_res <- data.frame(matrix(unlist(dbnpServers), nrow=length(dbnpServers), byrow=T ), stringsAsFactors = FALSE )
          names(df_res) <- c("url", "user", "password", "skey", "id")
          
          cat(paste0(Sys.time(),"  ","User ",username," can access the following DBNP servers ",toString(df_res['url']), "\n"), file=LOG_FILE, append=TRUE)
          globalValues$dbnp.servers <- df_res
        }else{
          cat(paste0(Sys.time(),"  ","User ",username," don't have any DBNP server.\n"), file=LOG_FILE, append=TRUE)
          globalValues$dbnp.servers <- data.frame(url=character(0), user=character(0), password=character(0), skey=character(0), id=character(0))
        }
        
        ############## SETTING OPAL SERVERS ##########################
        if(length(opalServers) > 0){
          df_res <- data.frame(matrix(unlist(opalServers), nrow=length(opalServers), byrow=T ), stringsAsFactors = FALSE )
          names(df_res) <- c("id","server_name", "url", "user", "password")
          
          cat(paste0(Sys.time(),"  ","User ",username," can access the following OPAL servers ",toString(df_res['url']), "\n"), file=LOG_FILE, append=TRUE)
          globalValues$opal.servers <- df_res
        }else{
          cat(paste0(Sys.time(),"  ","User ",username," don't have any OPAL server.\n"), file=LOG_FILE, append=TRUE)
          globalValues$dbnp.servers <- data.frame(id=character(0), server_name=character(0), url=character(0), user=character(0), password=character(0))
        }
      }
      shiny::observeEvent(servers(), {
        load.data.servers(servers())
      })
      # === END URL PARSING AND DATABASE CONNECTION
      # === on FLUSH
      # run This will run every time that shiny flushes the reactive system 
      # such as when the selectInput is changed by the user.
      # What is important is that it will run before input$XX gets a new value 
      # so the value has changed at the selectizeInput but the UI is still not updated
      session$onFlush(once=FALSE, function(){
        shiny::isolate({ 
          # this is needed to remember the previous selection 
          # after changing analysis type in the analysis tab
          globalValues$old_var_x<-input$var_x 
          globalValues$old_var_y<-input$var_y
          
          #  globalValues$old_projs <- list()
          #  lapply(get.opal.servers()$server, function(serv) {
          #    globalValues$old_projs[serv] <- input[[paste0('dsProjectList', serv)]]
          # })
          
          #  globalValues$old_tbls <- list()
          #  lapply(get.opal.servers()$server, function(serv) {
          #    globalValues$old_projs[serv] <- input[[paste0('dsProjectTablesList', serv)]]
          #  })
        })
      })
      # === END on FLUSH
      # === SHINY-DEPENDENT UTILS
      login.dataframe.row <- function(servers, dbnp.logins, opallogin, servName, servCombo){
        # ---------------------------------- PHENOTYPE DATABASE EXPORT ---------------------
        # if there exist a phenotype study selected export to opal and add it to login dataframe
        phenotype_prj <- c()
        phenotype_tbl <- c()
        if(   !is.null(input[[paste0("dbnpStudy", servCombo)]]) && input[[paste0("dbnpStudy", servCombo)]] != 'na' 
              && !is.null(input[[paste0("studyAssayList", servCombo)]]) && input[[paste0("studyAssayList", servCombo)]] != 'na' ){
          server_id <- servers[grepl(paste0('^',servName,'$'), servers$server_name), 'id']
          dbnplogin <- dbnp.logins[server_id, ]
          shiny::withProgress(message='Exporting from DBNP server', style='old', value=3, {
            shiny::incProgress(1, detail = paste0('Logging into DBNP server ',servName))
            dbnp.login(dbnplogin)
            shiny::incProgress(1, detail = paste0('Exporting ',servName,' study assay to OPAL..'))
            export_info <- PhenotypeDatabaseRClient::exportAssayToOpal(input[[paste0("studyAssayList", servCombo)]])
          })
          if( grepl("failed", export_info$status) )
            stop(paste0("Export of study assay ", input[[paste0("studyAssayList", servCombo)]], " failed. Stop."))
          phenotype_table <- c(paste0(export_info$project, '.', export_info$table))
          phenotype_prj <- export_info$project
          phenotype_tbl <- export_info$table
        }else
          phenotype_table <- c()
        
        opal_prj <- c()
        opal_tbl <- c()
        if(   !is.null(input[[paste0("dsProjectList", servCombo)]]) && input[[paste0("dsProjectList", servCombo)]] != 'na' 
              && !is.null(input[[paste0("dsProjectTablesList", servCombo)]]) && input[[paste0("dsProjectTablesList", servCombo)]] != 'na' ){
          opal_prj <- input[[paste0("dsProjectList", servCombo)]]
          opal_tbl <- input[[paste0("dsProjectTablesList", servCombo)]]
          opal_table <- c(paste0(opal_prj, '.', opal_tbl))
        }else
          opal_table <- c()
        
        tables <- c(
          opal_table,
          # Add phenotype exported table to list of tables used for analysis
          phenotype_table
        )
        prjs <- c(
          opal_prj,
          phenotype_prj
        )
        table_names <- c(
          opal_tbl,
          phenotype_tbl
        )
        if( length(tables) > 0 )
          cbind.data.frame(
            opallogin, 
            table=tables,
            project=prjs ,
            table_name=table_names
          )
      }
      # === END SHINY-DEPENDENT UTILS
      # === OBSERVER
      # observers are the first blocks executed in the shinyserver
      # they will invoke and react to reactive blocks as soon as they change
      
      # observe for the longest operation: loading the variables and getting the types
      # triggered after datashield.login by the reactive function that reads the list of variables 
      
      
      ## OBSERVERS FOR TEST BIOMARKERS BUTTON ###########
      shiny::observeEvent(input$analysisFields, {
        if( !is.null(input$analysisFields) && input$analysisFields != "")
          shinyjs::enable("testBiomarker")
        else
          shinyjs::disable("testBiomarker")
      })
      
      # if inputs change -> show FALSE
      shiny::observe({
        # dependencies
        input$dsProjectTablesList 
        input$analysis
        input$var_target
        input$var_time
        input$var_explanatory
        input$null_var_explanatory
        input$familyFunction
        
        globalValues$showAnalysis <- FALSE
      })
      
      # if button clicked -> show TRUE
      shiny::observe({
        if( !is.null(input$analyze) && input$analyze )
          globalValues$showAnalysis <- TRUE
      })
      
      # if inputs change -> show FALSE
      shiny::observe({
        #dependencies
        input$var_y
        input$var_x
        input$plotType
        input$intervals
        input$vars
        input$x_measure
        input$y_measure
        input$size_measure
        input$vars_x
        input$vars_y
        input$cca_lambda1
        input$cca_lambda2
        input$knn_vars_x
        input$knn_clusters
        input$knn_max_iter
        input$knn_start
        
        globalValues$showPlot <- FALSE
        globalValues$last_RFS <- NA
        globalValues$last_KNN <- NA
      })
      
      # if button clicked -> show TRUE
      shiny::observe({
        if( !is.null(input$plot) && input$plot )
          globalValues$showPlot <- TRUE
      })
      # === END OBSERVER
      # === REACTIVE
      # reactive block will be re-executed only on input change
      # otherwise it will cache the result and returns each time the same value
      # reactive block will be re-executed only on input change
      # otherwise it will cache the result and returns each time the same value
      
      get.ds.login <- shiny::reactive({
        if ( is.load.ready() ) {
          my_login <- get.login.dataframe()
          cat(paste0(Sys.time(),"  ","User ",globalValues$username," is loading data for federated analysis from ", my_login$server, ": ", my_login$url, "\n"), file=LOG_FILE, append=TRUE)
          
          builder <- DSI::newDSLoginBuilder()
          tables <- list()
          servers <- c()
          for(i in seq(nrow(my_login))){
            # login information
            curserver <- my_login[i, ]$server_name;
            if( curserver %in% servers ){
              curserver <- paste0(curserver, i)
            }
            builder$append(server=curserver, url=my_login[i, ]$url,
                           user=my_login[i, ]$user, password=my_login[i, ]$password)
            servers <- c(servers, curserver)
            
            # tables to load
            tables[[i]] <- as.character(my_login[i, ]$table)
            names(tables)[[i]] <- as.character(curserver)
          }
          
          logindata <- builder$build()
          
          dslogin <- tryCatch({
            DSI::datashield.login(logins=logindata)
          },
          error=function(cond){
            errs <- DSI::datashield.errors()
            if( is.null(errs) )
              stop(cond)
            else
              stop(errs)
          },
          warning=function(cond){
            cat(paste0(Sys.time()," ", cond,'\n'), file=LOG_FILE, append=TRUE)
          }
          )
          
          #dslogin <- datashield.login(logins=my_login, assign=TRUE, symbol = 'D')
          # assign Opal tables to symbol D
          tryCatch({
            DSI::datashield.assign.table(dslogin, symbol = "D",
                                         table = tables)
          },
          error=function(cond){
            errs <- DSI::datashield.errors()
            if( is.null(errs) )
              stop(cond)
            else
              stop(errs)
          },
          warning=function(cond){
            cat(paste0(Sys.time()," ", cond,'\n'), file=LOG_FILE, append=TRUE)
          })
          
          dslogin
        }
      })
      
      get.ds.size <- shiny::reactive({
        o <- get.ds.login()
        
        tryCatch({
          size <- dsBaseClient::ds.dim('D', type='combine', datasources=o)
        },
        error=function(cond){
          errs <- DSI::datashield.errors()
          if( is.null(errs) )
            stop(cond)
          else
            stop(errs)
        },
        warning=function(cond){
          cat(paste0(Sys.time()," ", cond,'\n'), file=LOG_FILE, append=TRUE)
        })
        
        size[[1]]
      })
      
      get.dbnp.servers <- shiny::reactive({
        globalValues$dbnp.servers
      })
      
      get.dbnp.logins <- shiny::reactive({
        servers <- get.opal.servers()
        dbnp <- get.dbnp.servers()
        dbnp.logins <- dbnp[dbnp$id %in% servers$id, ]  
        row.names(dbnp.logins) <- dbnp.logins$id
        dbnp.logins
      })
      
      get.opal.servers <- shiny::reactive({
        globalValues$opal.servers
      })
      
      get.login.dataframe <- shiny::reactive({
        # dependency, update login dataframe on each load
        input$load
        
        servers <- get.opal.servers()
        dbnp.logins <- get.dbnp.logins()
        do.call(rbind, 
                #bind everything by row
                lapply(get.opal.servers()$server, function(servName) {
                  opallogin <- servers[servers$server %in% servName, ]
                  n_extra_combo <- extraComboBoxes[[servName]] - 1;
                  
                  # bind by row multiple tables selected from the same server
                  extra <- do.call(rbind,
                                   # + extra login combo, if present
                                   lapply(seq(to=n_extra_combo, length.out = n_extra_combo), function(n){
                                     login.dataframe.row(servers, dbnp.logins, opallogin, servName, paste0(servName, n))
                                   })
                  )
                  
                  rbind(
                    # base login combo
                    login.dataframe.row(servers, dbnp.logins, opallogin, servName, servName),
                    extra
                  )
                })
        )
      })
      
      configured.servers <- shiny::reactive({
        servers <- get.opal.servers();
        
        sapply(servers$id, function(server_id) {
          server <- servers[grepl(server_id, servers$id), ]
          servName <- server$server_name
          
          proj_selected <- !is.null(input[[paste0("dsProjectList", servName)]]) && input[[paste0("dsProjectList", servName)]] != 'na'
          tbl_selected <- !is.null(input[[paste0("dsProjectTablesList", servName)]]) && input[[paste0("dsProjectTablesList", servName)]] != 'na'
          dbnp_proj_selected <- !is.null(input[[paste0("dbnpStudy", servName)]]) && input[[paste0("dbnpStudy", servName)]] != 'na'
          dbnp_tbl_selected <- !is.null(input[[paste0("studyAssayList", servName)]]) && input[[paste0("studyAssayList", servName)]] != 'na'
          if((proj_selected && tbl_selected) || (dbnp_proj_selected && dbnp_tbl_selected)) 
            TRUE
          else
            FALSE
        })
      })
      
      is.load.ready <- shiny::reactive({
        # any server configured?
        n_servers <- configured.servers()
        
        any(n_servers)
      })
      
      load.opal.index.frame <- shiny::reactive({
        # metadata table 
        if( is.null(input$loadMeta) || !input$loadMeta)
          return(NULL)
        
        shiny::isolate({
          if( !is.null(input$metadataTablesList) && input$metadataTablesList != "na"){
            my_login <- get.login.dataframe()
            if(is.null(my_login))
              return(NULL)
            progress <- shiny::Progress$new(session, min=0, max=5, style='old')
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            encoded_metadata <- strsplit(input$metadataTablesList, '|', fixed=T)[[1]]
            servName <- encoded_metadata[1]
            metadata_table <- encoded_metadata[2]
            # select login, in case of multiple matches get first (the table is metadata_table)
            login <- my_login[which(servName == my_login$server_name), ][1, ]
            progress$set(value = 2,
                         message = 'Logging in into OPAL data servers...',
                         detail = 'This may take a while...')
            # do login
            o <- NULL
            tryCatch({
              o <- doOpalLogin(login)
            }, error=function(e){
              str <- paste0(Sys.time(),"  ","ERROR: Connection to \n",my_login$server_name," (", my_login$url ,") for user ",globalValues$username, " failed: ", e, "\n")
              cat(str, file=LOG_FILE, append=TRUE)
              stop(str)
            })
            # get whole table
            works <- raw(0)
            cat(paste0(Sys.time(), " ", "User ", globalValues$username, " is loading metadata table ", metadata_table, " from server ", login$server_name, '\n'), file=LOG_FILE, append=TRUE)
            progress$inc(amount = 2, detail = 'Login successful, trying to access data...')
            tryCatch({
              opalr::opal.assign(o,'indextable', metadata_table)
              works <- opalr::opal.execute(o, 'indextable')[[1]]
            }, error=function(e){
              str <- paste0(Sys.time(),"  ","ERROR: Loading of metadata table \n",metadata_table," (", servName ,") for user ",globalValues$username, " failed: ", e, "\n")
              cat(str, file=LOG_FILE, append=TRUE)
              stop(str)
            },
            finally = {
              progress$inc(amount = 1, detail = 'Done')
            })
            return(works)
          }else
            return(NULL)
        })
      })
      
      load.opal.data.frames <- shiny::reactive({
        # this block depends only on input$load button
        # if 0
        if( is.null(input$load) || !input$load)
          return(NULL)
        
        shiny::isolate({
          my_login <- get.login.dataframe()
          if(is.null(my_login))
            return(NULL)
          n_logins <- dim(my_login)[1]
          progress <- shiny::Progress$new(session, min=0, max=n_logins+1, style='old')
          # Make sure it closes when we exit this reactive, even if there's an error
          on.exit(progress$close())
          progress$set(value = 1,
                       message = 'Logging in into OPAL data servers...',
                       detail = 'This may take a while...')
          # do login
          o <- NULL
          tryCatch({
            o <- doOpalLogin(my_login)
          }, error=function(e){
            str <- paste0(Sys.time(),"  ","ERROR: Connection to \n",my_login$server_name," (", my_login$url ,") for user ",globalValues$username, " failed: ", e, "\n")
            cat(str, file=LOG_FILE, append=TRUE)
            stop(str)
          })
          header.names <- NULL
          # get whole table
          results <- lapply(seq(nrow(my_login)), function(n){
            opalServerName <- my_login[n, 'server_name']
            cur_login <- my_login[n, ]
            works <- raw(0)
            # if multiple returned assign and return all of them
            curOpalLoginObject <- o[[opalServerName]]
            progress$inc(amount = 1, detail = paste0('Login successful, trying to access data from ',opalServerName,'...'))
            opalr::opal.assign(curOpalLoginObject, 'fulltable',toString(cur_login$table))
            works <- opalr::opal.execute(curOpalLoginObject, 'fulltable')
            # CHECK HEADERS SHOULD BE EXACTLY THE SAME
            if(is.null(header.names))
              header.names <<- names(works)
            else if(!all( names(works) == header.names ) ){
              str <- paste0(Sys.time(),"  ","ERROR: Can't execute the same analysis on data tables with different column names\n")
              cat(str, file=LOG_FILE, append=TRUE)
              stop(str)
            }
            works
          })
          names(results) <- names(o)
          progress$inc(amount = 1, detail = 'Done')
          
          return(results)
        })
      })
      
      # detect the list of variables in the study
      get.ds.project.table.variables <- shiny::reactive({
        # this block depends only on input$load button
        if( is.null(input$load) || !input$load)
          return(NULL)
        
        shiny::isolate({
          #tryCatch({
          progress <- shiny::Progress$new(session, min=0, max=5, style='old')
          on.exit(progress$close())
          
          progress$set(value = 1,
                       message = 'Logging into Federated Analysis data servers...',
                       detail = 'This may take a while...')
          
          dslogin <- get.ds.login()
          
          progress$inc(amount = 1, detail = 'Retrieving list of variable names...')
          
          progress$inc(amount = 1, detail = 'Retrieving dataset size...')
          size <- get.ds.size()
          
          # if dataset empty, useless to load it
          if( size[2] == 0 || size[1] == 0 )
            stop("Data source empty")
          
          cols <- tryCatch({
            dsBaseClient::ds.colnames('D', datasources=dslogin)[[1]]
          },
          error=function(cond){
            errs <- DSI::datashield.errors()
            if( is.null(errs) )
              stop(cond)
            else
              stop(errs)
          },
          warning=function(cond){
            cat(paste0(Sys.time()," ", cond,'\n'), file=LOG_FILE, append=TRUE)
          })
          
          ncols <- length(cols)
          
          progress$inc(amount = 1, detail = 'Retrieving variable types...')
          #cols <- list('x1','x2','y')
          # helper for get.ds.project.table.variables
          get.col.types <- function(colname){
            progress$inc(amount = 1/ncols, detail=paste0('Setting up \'', colname, '\'', ' data...'))
            
            classes <- tryCatch({
              dsBaseClient::ds.class(x=paste0('D$',colname), datasources = dslogin)[[1]]
            },
            error=function(cond){
              errs <- DSI::datashield.errors()
              if( is.null(errs) )
                stop(cond)
              else
                stop(errs)
            },
            warning=function(cond){
              cat(paste0(Sys.time()," ", cond,'\n'), file=LOG_FILE, append=TRUE)
            })
            
            # check that all the classes are the same
            if( !all(classes == classes[[1]]) ){
              errorMsg <- paste0("The input column '",colname,"' is not of the same class in all studies! [",toString( paste0(names(classes)," has type ",classes) ),"]")
              stop(errorMsg)
            }
            return(classes)
          }
          coltypes <- sapply( cols, get.col.types)
          # select only first row, it is sufficient because all the rows are equal!
          # otherwise the check in get.col.types would have thrown an error
          if( !is.null(nrow(coltypes)) )
            #if coltypes has more than 1 row (in that case nrow will return NULL)
            coltypes <- coltypes[1, ]
          
          adf <- data.frame(type=unlist(coltypes))
          row.names(adf) <- cols
          return(adf)
        })
      })
      
      # get the model for the given input parameters
      get.ds.glm.full.model <- shiny::reactive({
        # take dependency on button, execute something only if his "clicked" value is greater or equal to 1
        if(!globalValues$showAnalysis)
          return(NULL)
        
        model <- shiny::isolate({
          # do not take dependency on these input objects
          if( input$var_explanatory != '' && is.load.ready() ){
            o <- get.ds.login()
            vars <- get.ds.project.table.variables()
            model_vars <- get.model.vars(o, vars, input$var_explanatory, input$var_target, input$analysis, input$familyFunction)
            
            switch (input$analysis,
                    
                    'lm' = {
                      # lm model
                      plotData <- tryCatch({
                        dsBaseClient::ds.glm(formula=paste0(model_vars$output," ~ ", model_vars$input), 
                                             family='gaussian',
                                             maxit=glm_max_iterations,
                                             datasources = o)
                      },
                      error=function(cond){
                        errs <- DSI::datashield.errors()
                        if( is.null(errs) )
                          stop(cond)
                        else
                          stop(errs)
                      },
                      warning=function(cond){
                        cat(paste0(Sys.time()," ", cond,'\n'), file=LOG_FILE, append=TRUE)
                      })
                      
                      #plotData <- glm(formula=paste0(input$var_target, ' ~ ', input$var_explanatory), family=gaussian,
                      #                data=fetchedData)
                    },
                    
                    'glm' = {
                      # glm model
                      plotData <- tryCatch({
                        dsBaseClient::ds.glm(formula=paste0(model_vars$output," ~ ", model_vars$input), 
                                             family=input$familyFunction,
                                             maxit=glm_max_iterations,
                                             datasources = o)
                      },
                      error=function(cond){
                        errs <- DSI::datashield.errors()
                        if( is.null(errs) )
                          stop(cond)
                        else
                          stop(errs)
                      },
                      warning=function(cond){
                        cat(paste0(Sys.time()," ", cond,'\n'), file=LOG_FILE, append=TRUE)
                      })
                      
                      #plotData <- glm(formula=paste0(input$var_target, ' ~ ', input$var_explanatory), family=gaussian(link=input$linkFunction),
                      #                data=fetchedData)
                    }
            )
            
            return(plotData)
          }
        })
        
        return(model)
      })
      
      # get the model for the given input parameters
      get.ds.glm.null.model <- shiny::reactive({
        # take dependency on button, execute something only if his "clicked" value is greater or equal to 1
        if(!globalValues$showAnalysis || input$analysis != 'lm')
          return(NULL)
        
        model <- shiny::isolate({
          # do not take dependency on these input objects
          if(  input$null_var_explanatory != '' &&  is.load.ready() ){
            o <- get.ds.login()
            vars <- get.ds.project.table.variables()
            output_var <- get.model.output.var(input$var_target, input$analysis, 'gaussian', vars, o)
            model_vars <- get.model.vars(o, vars, input$null_var_explanatory, input$var_target, input$analysis, 'gaussian')
            
            switch (input$analysis,
                    
                    'lm' = {
                      plotData <- tryCatch({
                        dsBaseClient::ds.glm(formula=paste0(output_var," ~ ", model_vars$input), 
                                             family='gaussian',
                                             maxit=glm_max_iterations,
                                             datasources = o)
                      },
                      error=function(cond){
                        errs <- DSI::datashield.errors()
                        if( is.null(errs) )
                          stop(cond)
                        else
                          stop(errs)
                      },
                      warning=function(cond){
                        cat(paste0(Sys.time()," ", cond,'\n'), file=LOG_FILE, append=TRUE)
                      })
                      
                      #plotData <- glm(formula=paste0(input$var_target, ' ~ ', input$var_explanatory), family=gaussian,
                      #                data=fetchedData)
                    },
                    
                    'glm' = {
                      plotData <- tryCatch({
                        dsBaseClient::ds.glm(formula=paste0(output_var," ~ ", model_vars$input), 
                                             family=input$familyFunction,
                                             maxit=glm_max_iterations,
                                             datasources = o)
                      },
                      error=function(cond){
                        errs <- DSI::datashield.errors()
                        if( is.null(errs) )
                          stop(cond)
                        else
                          stop(errs)
                      },
                      warning=function(cond){
                        cat(paste0(Sys.time()," ", cond,'\n'), file=LOG_FILE, append=TRUE)
                      })
                      
                      #plotData <- glm(formula=paste0(input$var_target, ' ~ ', input$var_explanatory), family=gaussian(link=input$linkFunction),
                      #                data=fetchedData)
                    }
            )
            
            return(plotData)
          }
        })
        
        return(model)
      })
      
      is.opal.analysis.ready <- shiny::reactive({
        df <- load.opal.data.frames()
        if(is.null(df))
          return(FALSE)
        else if( length(df) == 0 ) 
          return(FALSE)
        else if( length(df[[1]]) == 0 ) 
          return(FALSE)
        else 
          return(TRUE)
      })
      
      is.analysis.ready <- shiny::reactive({
        !is.null(get.ds.project.table.variables()) && !is.null(get.ds.size())
      })
      
      # ------------- VARIABLES USED FOR CONDITIONALPANEL
      output$analysisReady <- shiny::reactive({
        return(is.load.ready() && is.analysis.ready())
      })
      shiny::outputOptions(output, 'analysisReady', suspendWhenHidden = FALSE)
      # === END REACTIVE
      # === PHENOTYPE DATA SOURCE      
      output$dbnpProjects <- shiny::renderUI({
        
        if (!is.null(input$dbnpServer)) {
          servers <- get.dbnp.servers()
          logins <- servers[servers$server %in% input$dbnpServer, ]
          ## Specify the Phenotype Database instance
          PhenotypeDatabaseRClient::setPhenotypeDatabaseBaseUrl(login$url)
          user = "zefoteus"
          pass = "v;4:xSx9#"
          skey = "e1fb2eea-fdd2-4be6-afc7-766ed049ed9c"
          PhenotypeDatabaseRClient::authenticate(login$user, pass, skey)
          
          ## Get available studies
          PhenotypeDatabaseRClient::getStudies()
          
        }else{
          shiny::h4("No opal servers selected for analysis")
        }
        
      })
      
      output$DBNPstudyAssay <- shiny::renderUI({
        if (input$dbnpStudy != "na") {
          shiny::selectInput(ns("studyAssayList"),
                             label = "Study Assays",
                             choices = get.dbnp.study.assay.names(input$dbnpStudy)
          )  
        }else{
          shiny::h4("No phenotype database selected for analysis")
        }
      })
      # === END PHENOTYPE DATA SOURCE      
      # === OPAL DATA SOURCE      
      # loop over the servers
      # Initialize the intial progress bar
      init.progress <- shiny::Progress$new(session, min=0, max=2,style='old')
      init.progress$set(value = 1,
                        message = 'Connecting to DASH-IN servers...',
                        detail = 'This may take a while...')
      
      globalValues$serversLoaded <- FALSE
      
      render.project.combo <- function(prjs, servName){
        ch_prj <- c("na", levels(prjs$name))
        if( length(ch_prj) > 1 )
          names(ch_prj) <- c( "[Select an OPAL project]", ch_prj[2:length(ch_prj)] )
        else
          # at least one element exists
          names(ch_prj) <- c( "[No OPAL projects]" )
        
        shiny::selectInput(ns(paste0("dsProjectList", servName)),
                           label = shiny::span(style='font-weight: normal;', "Opal Projects"),
                           choices = ch_prj)
      }
      
      render.table.combo <- function(prjsdf, prj, servName){
        if( is.null(prj) || !any(prjsdf$name == prj) ){
          ch_tbl <- c("na")
          names(ch_tbl) <- c( "[No OPAL tables]" )
        }else{
          tables <- prjsdf$tables[[which(prjsdf$name == prj)]]
          tablesarray <- strsplit( as.character(tables), "|", fixed=T)[[1]]
          ch_tbl <- c("na", tablesarray)
          names(ch_tbl) <- c( "[Select a table]", tablesarray )
        }
        
        shiny::selectInput(ns(paste0("dsProjectTablesList", servName)),
                           label = shiny::span(style='font-weight: normal;', "Data tables"),
                           choices = ch_tbl)
      }
      
      output$serversList <- shiny::renderUI({
        servers <- get.opal.servers()
        dbnp <- get.dbnp.servers()
        
        if( is.null(servers) )
          return(shiny::fluidRow(shinydashboard::box(width=12, title = "Error", status = "danger", solidHeader = TRUE, 
                                     shiny::HTML(paste0("<H2>Data Servers missing, please configure at least one server</H2>")))))
        
        lapply(servers$id, function(server_id) {
          dbnp.logins <- NULL
          server <- servers[grepl(server_id, servers$id), ]
          servName <- server$server_name
          # remove spaces from id
          servNameId <- gsub(' ', '_', servName)
          
          if( !is.null(dbnp) ){
            dbnp.logins <- dbnp[grepl(server_id, dbnp$id), ]  
            row.names(dbnp.logins) <- dbnp.logins$id
          }
          cat(paste0(Sys.time(),"  Connecting to ",server$server_name," for user ",globalValues$username,'\n'), file=LOG_FILE, append=TRUE)
          o <- prjs <- NULL
          tryCatch({
            o <- doOpalLogin(server)
            #init.progress$inc(amount = 1/length(servers))
            prjs <- get.opal.projects(o)
          }, error=function(e){
            str <- paste0(Sys.time(),"  ","ERROR: Connection to \n",server$server_name," (", server$url ,") for user ",globalValues$username, " failed: ", e, "\n")
            cat(str, file=LOG_FILE, append=TRUE)
            stop(str)
          })
          
          output[[paste0('conf', servName)]] <- shiny::renderUI({
            shiny::strong(paste0("Data Server ",servName))
          })
          
          output[[paste0('confProj',servName)]] <- shiny::renderUI({
            render.project.combo(prjs, servName)
          })
          
          output[[paste0('confTable',servName)]] <- shiny::renderUI({
            prj <- input[[paste0("dsProjectList", servName)]]
            render.table.combo(prjs, prj, servName)
          })
          
          output[[paste0('confDBNPProj', servName)]] <- shiny::renderUI({
            if( !is.null(dbnp.logins) && nrow(dbnp.logins)>0 ){
              login_status <- dbnp.login(dbnp.logins)
              
              ## Get available studies
              shiny::selectInput(ns(paste0("dbnpStudy", servName)),
                                 label = shiny::span(style='font-weight: normal;', "Phenotype Database Studies"),
                                 choices = c("[Don't use a DBNP study]" = "na",
                                             get.dbnp.study.names(PhenotypeDatabaseRClient::getStudies()) 
                                 )
              )
            }
          })
          
          output[[paste0('confDBNPTable', servName)]] <- shiny::renderUI({
            if (!is.null(input[[paste0("dbnpStudy",servName)]]) && input[[paste0("dbnpStudy",servName)]] != "na") {
              login <- dbnp.logins[server$id, ]
              dbnp.login(login)
              choices <- get.dbnp.study.assay.names(input[[paste0("dbnpStudy",servName)]])
              if( length(choices) == 0 )
                choices <- c('[No phenotype table accessible]' = 'na')
              shiny::selectInput(ns(paste0("studyAssayList", servName)),
                                 label = shiny::span(style='font-weight: normal;', "Study Assays"),
                                 choices = choices
              )  
            }
          })
          
          #### DYNAMIC +/- buttons 
          extraComboBoxes[[servName]] <<- 1
          
          shiny::observeEvent(input[[paste0("newTable",servName)]], {
            #### on + button click ######
            shiny::isolate({
              n <- extraComboBoxes[[servName]]
              id <- paste0('extraTable', servNameId, n)
              
              output[[paste0('confProj',servName, n)]] <- shiny::renderUI({
                render.project.combo(prjs, paste0(servName,n))
              })
              
              output[[paste0('confTable',servName, n)]] <- shiny::renderUI({
                prj <- input[[paste0("dsProjectList", servName, n)]]
                render.table.combo(prjs, prj, paste0(servName,n))
              })
              
              shiny::insertUI(
                selector = paste0('#nextTable',servNameId),
                ui = shiny::tags$div(
                  id = id,
                  shiny::fluidRow(
                    shiny::column(width=5,
                                  shiny::uiOutput(ns(paste0('confProj', servName, n)))
                    ),
                    shiny::column(width=6,
                                  shiny::uiOutput(ns(paste0('confTable', servName, n)))
                    )
                  )
                )
              )
              
              extraComboBoxes[[servName]] <<- n + 1
            })
          })
          
          shiny::observeEvent(input[[paste0("removeTable",servName)]], {
            ##### on - button click ###############
            shiny::isolate({
              if(extraComboBoxes[[servName]] <= 1)
                return()
              
              extraComboBoxes[[servName]] <<- extraComboBoxes[[servName]] - 1
              n <- extraComboBoxes[[servName]]
              id <- paste0('extraTable', servNameId, n)
              shiny::removeUI(
                ## pass in appropriate div id
                selector = paste0('#', id)
              )
              
            })
          })
          
          cat(paste0(Sys.time()," Loaded UI for server ",server$server_name," for user ",globalValues$username,'\n'), file=LOG_FILE, append=TRUE)
          #### RETURNED OBJECTS (FOREACH LAPPLY) TWO ROWS WITH DYNAMIC UIOUTPUT ##################
          shiny::fluidRow(
            shinydashboard::box(width=12, 
                                list(
                                  shiny::uiOutput(ns(paste0('conf', servName))),
                                  shiny::fluidRow(
                                    shiny::column(width=5,
                                                  shiny::uiOutput(ns(paste0("confProj",servName)))
                                    ),
                                    shiny::column(width=6,
                                                  shiny::uiOutput(ns(paste0("confTable",servName)))
                                    ),
                                    shiny::column(width=1,
                                                  shiny::actionButton(ns(paste0("newTable",servName)), "+"),
                                                  shiny::actionButton(ns(paste0("removeTable",servName)), "-")
                                    )
                                  ),
                                  shiny::fluidRow(
                                    shiny::column(width=5,
                                                  shiny::uiOutput(ns(paste0('confDBNPProj', servName)))
                                    ),
                                    shiny::column(width=6,
                                                  shiny::uiOutput(ns(paste0('confDBNPTable', servName)))
                                    )
                                  ),
                                  shiny::fluidRow(
                                    shiny::column(width=12,
                                                  # anchor to add new set of combo proj/table
                                                  shiny::tags$div(id = paste0('nextTable',servNameId))
                                    )
                                  )
                                )
            )
            # end first fluid row, server selection    
          )
        })
      })
      
      output$dsProjects <- shiny::renderUI({
        
        if (!is.null(input$opalServer)) {
          servers <- get.opal.servers()
          logins <- servers[servers$server_name %in% input$opalServer, ]
          o <- doOpalLogin(logins)
          prjs <- get.opal.projects(o)
          ch <- c("na", sapply(prjs, function(x) x$name))
          #ch <- list("na","toremove","toremove2","toremove3")
          if( length(ch) > 1 )
            names(ch) <- c( "[Select a common OPAL project]", ch[2:length(ch)] )
          else
            # at least one element exists
            names(ch) <- c( "[No common OPAL projects]" )
          
          shiny::selectInput(ns("dsProjectList"),
                             label = shiny::tagList(shiny::strong("Opal"), shiny::span(style='font-weight: normal;', " Projects")),
                             choices = ch
          )
        }else{
          shiny::h4("No opal servers selected for analysis")
        }
        
      })
      
      output$dsActions <- shiny::renderUI({
        if( is.load.ready() )
          btn <- shiny::actionButton(ns("load"), "Setup remote data tables")
        else
          btn <-shiny::helpText("Please select at least a data table above before proceeding.")
        
        btn
      })
      
      output$dsProjectTables <- shiny::renderUI({
        
        if (!is.null(input$opalServer) && !is.null(input$dsProjectList) && input$dsProjectList != 'na') {
          
          servers <- get.opal.servers()
          login <- servers[servers$server %in% input$opalServer, ]
          o <- doOpalLogin(login)
          prjs <- get.opal.projects(o)
          
          ch <- which( sapply(prjs, function(x) x$name) == input$dsProjectList )
          ch <- c("na", prjs[[ch]]$table)
          #ch <- c("na", list("table1"))
          if( length(ch) > 1 )
            names(ch) <- c( "[Select a common table]", ch[2:length(ch)] )
          else
            names(ch) <- c( "[No common OPAL tables]" )
          
          list(
            shiny::selectInput(ns("dsProjectTablesList"),
                               label = "Data tables",
                               choices = ch
            ),
            shiny::actionButton(ns("load"), "Load")
          )
        }
      })
      
      output$dsProjectTableLoadStatus <- shiny::renderUI({
        datashielderr <- NULL
        datashield <- FALSE
        
        # this block depends only on input$load button
        # if 0
        if( !is.load.ready() || is.null(input$load) || !input$load )
          return(shiny::span())
        
        tryCatch({
          shiny::span("Loading data sources for federated analysis...")
          datashield <- is.analysis.ready()
        }, error = function(e){
          datashielderr <<- shiny::span(style="color:red; margin-top: 5px;", paste0("Failed to load datasources for federated analysis: ", e))
          cat(paste0(Sys.time(),"  ","User ",globalValues$username," got '",paste0("",e),"' loading data for federated analysis\n"), file=LOG_FILE, append=TRUE)
        })
        
        if(!datashield && is.null(datashielderr))
          datashielderr <- shiny::span(style="color:red; margin-top: 5px;", paste0("Failed to load datasources for federated analysis: ", "Unable to retrieve data"))
        if(datashield)
          shiny::tagList(shiny::span(style="color:green", "Data sources were successfully setup on remote servers! Visit "),
                         shiny::HTML("<span style='font-weight:bold; color:green'>Analysis</span> <span style='color:green'>section in the left side-pane.</span>"))
        else
          shiny::tagList(shiny::span(style="color:red", "There was an error loading data sources. "),
                         shiny::HTML("<div></div>"),
                         datashielderr
          )              
      })
      
      
      init.progress$close()
      globalValues$serversLoaded <- TRUE
      # === END OPAL DATA SOURCE      
      # === ANALYSIS
      output$generalInfos <- shiny::renderUI({
        if(!is.load.ready() || !is.analysis.ready())
          return(NULL);
        size <- get.ds.size()
        shiny::h4(paste0("Loaded data table with ",size[2], " variables and about ", size[1], " rows"))
      })
      
      output$plotType <- shiny::renderUI({
        if ( is.load.ready() && is.analysis.ready() ) {
          shiny::selectInput(
            ns("plotType"),
            label = "Activity",
            choices = list("Histogram"    = "hist",
                           "Contour Plot" = "contour",
                           "Heatmap"      = "heatmap",
                           "Box-plot"     = "boxplot",
                           "Principal Component Analysis" = "princomp",
                           "Random Forest" = "randomforest",
                           "Correlation"  = "correlation",
                           "Data imputation" = "vim",
                           "Analisys"     = "analisys")
          )
        } 
      })
      
      output$varXY <- shiny::renderUI({
        if ( !is.null(input$plotType) && is.analysis.ready() ) {
          # dataframe, rownames are the name of the variable
          # $type is the type of the variable
          vars <- get.ds.project.table.variables()
          varnames <- get.input.named.vars(vars)
          
          # check if previous selection is applicable
          old_var_x <- NULL
          old_var_y <- NULL
          if( !is.null(globalValues$old_var_x) && globalValues$old_var_x %in% varnames )
            old_var_x <- globalValues$old_var_x
          if( !is.null(globalValues$old_var_y) && globalValues$old_var_y %in% varnames )
            old_var_y <- globalValues$old_var_y
          
          list(
            shiny::conditionalPanel(
              condition = paste0("input['",ns('plotType'),"']  == 'hist' || input['",ns('plotType'),"']  == 'contour' || input['",ns('plotType'),"'] == 'heatmap'"),
              
              shiny::numericInput(ns("intervals"),
                                 "Number of intervals",
                                 value=10,
                                 min=3,
                                 step=1
              )
            ), 
            shiny::conditionalPanel(
              condition = paste0("input['",ns('plotType'),"'] == 'boxplot' || input['",ns('plotType'),"'] == 'vim'"),
              
              shiny::selectInput(ns("vars_x"), "Variates",
                                 choices=varnames,
                                 multiple=T
              )
            ),
            shiny::conditionalPanel(
              condition = paste0("input['",ns('plotType'),"'] == 'princomp'"),
              
              shiny::selectInput(ns("knn_vars_x"), "Variates",
                                 choices=varnames,
                                 multiple=T
              ),
              shiny::numericInput(ns("knn_clusters"),
                                  "Number of clusters to compute (K-NN)",
                                  value=3,
                                  min=2,
                                  step=1,
                                  max=50
              ),
              shiny::numericInput(ns("knn_max_iter"),
                                  "Maximum number of iterations (K-NN)",
                                  value=50,
                                  min=2,
                                  step=1,
                                  max=100
              ),
              shiny::numericInput(ns("knn_start"),
                                  "Starting number of clusters (K-NN)",
                                  value=60,
                                  min=2,
                                  step=1,
                                  max=100
              )
            ),
            shiny::conditionalPanel(
              condition = paste0("input['",ns('plotType'),"'] == 'correlation'"),
              
              shiny::selectInput(ns("cca_vars_x"), "Variates X",
                                 choices=varnames,
                                 multiple=T
              ),
              shiny::selectInput(ns("cca_vars_y"), "Variates Y",
                                 choices=varnames,
                                 multiple=T
              ),
              shiny::numericInput(ns("cca_lambda1"),
                                  "Regularization term for Cov(X) matrix",
                                  value=0.0,
                                  min=0.0,
                                  step=0.001,
                                  max=1.0
              ),
              shiny::numericInput(ns("cca_lambda2"),
                                  "Regularization term for Cov(Y) matrix",
                                  value=0.0,
                                  min=0.0,
                                  step=0.001,
                                  max=1.0
              )
            ),
            shiny::conditionalPanel(
              condition = paste0("input['",ns('plotType'),"'] == 'randomforest'"),
              shiny::selectInput(ns("var_y"),
                                 "Response factor",
                                 varnames,
                                 selected=old_var_y
              ),
              shiny::selectInput(ns("vars"), "Classification variables",
                                 choices=varnames,
                                 multiple=T
              ),
              shiny::selectInput(ns("x_measure"), "Importance plot X axis",
                                 choices=list("Mean min depth"="mean_min_depth",
                                              "Number of nodes"="no_of_nodes",
                                              "Number of trees"="no_of_trees",
                                              "Increase in MSE"="mse_increase",
                                              "Increase in node purity"="node_purity_increase",
                                              "Times as a root"="times_a_root",
                                              "P-value"="p_value"),
              ),
              shiny::selectInput(ns("y_measure"), "Importance plot Y axis",
                                 choices=list("Mean min depth"="mean_min_depth",
                                              "Number of nodes"="no_of_nodes",
                                              "Number of trees"="no_of_trees",
                                              "Increase in MSE"="mse_increase",
                                              "Increase in node purity"="node_purity_increase",
                                              "Times as a root"="times_a_root",
                                              "P-value"="p_value")
              ),
              shiny::selectInput(ns("size_measure"), "Importance plot dot-size",
                                 choices=list("Mean min depth"="mean_min_depth",
                                              "Number of nodes"="no_of_nodes",
                                              "Number of trees"="no_of_trees",
                                              "Increase in MSE"="mse_increase",
                                              "Increase in node purity"="node_purity_increase",
                                              "Times as a root"="times_a_root",
                                              "P-value"="p_value")
              ),
	      shiny::HTML("For further informations see <a href=\"https://modeloriented.github.io/randomForestExplainer/articles/randomForestExplainer.html#multi-way-importance-plot-1\" target=\"_blank\">Multi-way importance plot</a>")
            ),
            shiny::conditionalPanel(
              condition = paste0("input['",ns('plotType'),"']  == 'hist' || input['",ns('plotType'),"']  == 'contour' || input['",ns('plotType'),"'] == 'heatmap'"),
              
              shiny::selectInput(ns("var_x"),
                               "Data variable",
                               # filter varnames based on true/false value in fcols
                               varnames,
                               selected=old_var_x
              )
            ),
            shiny::conditionalPanel(
              condition = paste0("input['",ns('plotType'),"']  == 'contour' || input['",ns('plotType'),"'] == 'heatmap'"),
              
              shiny::selectInput(ns("var_y"),
                                 "Contrast variable",
                                 varnames,
                                 selected=old_var_y
              )
            ),
            shiny::actionButton(ns("plot"), "Run federated plot")
          )
          
        }
      })
      
      output$varAnalysis <- shiny::renderUI({
        # dataframe, rownames are the name of the variable
        # $type is the type of the variable
        vars <- get.ds.project.table.variables()
        varnames <- get.input.named.vars(vars)
        
        list(
          shiny::selectInput(
            ns("analysis"),
            label = "Analisys type:",
            choices = list("Linear Regression" = "lm",
                           "Generalized linear model (GLM)" = "glm",
                           "Survival analysis" = "coxp")
          ),
          
          shiny::conditionalPanel(
            condition = paste0("input['", ns('analysis'), "'] == 'glm'"),
            shiny::selectInput(
              ns("familyFunction"),
              label = "Output distribution",
              choices = list("Binomial (link=logistic)" = "binomial",
                             "Poisson (link=log)" = "poisson")
            )
          ),
          
          shiny::conditionalPanel(
            condition = paste0("input['", ns('analysis'), "'] == 'lm' ||  input['", ns('analysis'), "'] == 'glm' ||  input['", ns('analysis'), "'] == 'coxp'"),
            shiny::selectInput(
              ns("var_target"),
              label = "Dependent variable",
              choices = varnames
            )
          ),
          shiny::conditionalPanel(
            condition = paste0("input['", ns('analysis'), "'] == 'coxp'"),
            shiny::selectInput(
              ns("var_time"),
              label = "Time variable",
              choices = varnames
            )
          ),
          shiny::conditionalPanel(
            condition = paste0("input['", ns('analysis'), "'] == 'lm' ||  input['", ns('analysis'), "'] == 'glm' ||  input['", ns('analysis'), "'] == 'coxp'"),
            shiny::textAreaInput(
              ns("var_explanatory"),
              label = "Explanatory variables"
            )
          ),
          
          #----- UNCOMMENT TO ADD NULL MODEL SELECTION CAPABILITY ---------------
          shiny::conditionalPanel(
            condition = paste0("input['", ns('analysis'), "'] == 'lm'"),
            list(
              shiny::textAreaInput(
                ns("null_var_explanatory"),
                label = "Null explanatory variables",
                value = "1"
              ),
              shiny::helpText("Null model used to compute R^2 and F-test")
            )
          ),
          
          shiny::actionButton(ns("analyze"), "Run federated analysis")
          #end list
        )
      })
      
      # ------------------- ANALYSIS OUTPUT METHODS ------------------------------
      output$analysisStatus <- shiny::renderUI({
        
        if(!globalValues$showAnalysis)
          shiny::tagList(shiny::h4("Specify model parameters and click 'Run federated analysis'."),
                         shiny::br(),
                         shiny::h4("See the help box at the bottom for further instructions."))
        
      })
      
      output$plotStatus <- shiny::renderUI({
        if(!globalValues$showPlot)
          shiny::tagList(shiny::h4("Specify model parameters and click 'Run federated analysis'."))
      })
      
      output$plotDownload <- shiny::renderUI({
        if(!globalValues$showPlot || !is.load.ready() || !is.analysis.ready())
          return(shiny::tagList(shiny::span("")))
        shiny::isolate({
          if ( input$plotType == "randomforest") {
            downloadButton(ns("downloadRFS"), "Download Random Forests model as an RDS")
          }else if ( input$plotType == "princomp") {
              downloadButton(ns("downloadKNN"), "Download K-nearest neighbor model as an RDS")
          }else{
            shiny::tagList(shiny::span(""))
          }
        })
      })
      
      output$downloadRFS <- shiny::downloadHandler(
        filename = function() {
          o <- get.ds.login()
          paste0(paste(names(o), collapse="-"), "-RandomForests.rds")
        },
        content = function(file) {
          saveRDS(globalValues$last_RFS, file=file)
        }
      )
      output$downloadKNN <- shiny::downloadHandler(
        filename = function() {
          o <- get.ds.login()
          paste0(paste(names(o), collapse="-"), "-KNN.rds")
        },
        content = function(file) {
          saveRDS(globalValues$last_KNN, file=file)
        }
      )
      
      # plot explorative analysis
      output$distPlot <- shiny::renderPlot({
        if(!globalValues$showPlot || !is.load.ready() || !is.analysis.ready())
          return(NULL)
        
        o <- get.ds.login()
        # do not take dependency on these objects
        shiny::isolate({
          if ( !is.null(input$plotType) && !is.null(input$var_x) && input$plotType != 'analisys' ) {
            vars <- get.ds.project.table.variables()

            if ( input$plotType == "hist") {
              cat(paste0(Sys.time(),"  ","User ",globalValues$username," is analyzing ", input$var_x," with an histogram plot", "\n"), file=LOG_FILE, append=TRUE)
              x_var <- get.var.as.numeric(o, vars, input$var_x)
              
              h  <- tryCatch({
                dsBaseClient::ds.histogram(x = x_var, num.breaks = input$intervals, 
                                           method='probabilistic',
                                           type='combine',
                                           noise = 0.26,
                                           vertical.axis='Frequency',
                                           datasources = o)
              },
              error=function(cond){
                errs <- DSI::datashield.errors()
                if( is.null(errs) )
                  stop(cond)
                else
                  stop(errs)
              },
              warning=function(cond){
                cat(paste0(Sys.time()," ", cond,'\n'), file=LOG_FILE, append=TRUE)
              })
              
              if( is.null(h) ){
                stop("DataSHIELD plot returned an empty response")
              }else if ( class(h) == "list" ) {
                x <- h[[1]]$mids
                counts <- Reduce("+", lapply(h, function(el){ el$counts }))
                density <- Reduce("+", lapply(h, function(el){ el$density }))
              } else {
                x <- h$mids
                counts <- h$counts
                density <- h$density
              }
              max_density <- max(density)
              wdata = data.frame(x=x, frequency=counts, density=density)
              
              # 1. Create the histogram plot
              phist <- ggpubr::ggbarplot(
                wdata, x = "x", y="frequency",
                add = "mean", rug = TRUE,
                fill = "lightgray"
              ) + ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                                 axis.text.x=ggplot2::element_blank(),
                                 axis.ticks.x=ggplot2::element_blank())
              
              # 2. Create the density plot with y-axis on the right
              # Remove x axis elements
              pdensity <- ggpubr::ggline(
                wdata, x = "x", y = "density", 
                color= "lightgray",
                alpha = 0
              ) +
                ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, max_density)), position = "right")  +
                cowplot::theme_half_open(11, rel_small = 1) +
                ggpubr::rremove("x.axis")+
                ggpubr::rremove("xlab") +
                ggpubr::rremove("x.text") +
                ggpubr::rremove("x.ticks") +
                ggpubr::rremove("legend")
              
              # 3. Align the two plots and then overlay them.
              #aligned_plots <- align_plots(phist, pdensity, align="hv", axis="tblr")
              cowplot::ggdraw(phist)
              
              #hist(x=x, y=counts,
              #       main = ifelse(input$title  != "", input$title, paste("Histogram of", input$var_x)),
              #       xlab = ifelse(input$xlabel != "", input$xlabel, input$var_x),
              #       ylab = ifelse(input$ylabel != "", input$ylabel, "Frequency"),
              #       xlim = c(0,max(x))
              #     )
              
            } else if ( input$plotType == "contour") {
              cat(paste0(Sys.time(),"  ","User ",globalValues$username," is analyzing ", input$var_x," against ", input$var_y," with an contour plot", "\n"), file=LOG_FILE, append=TRUE)
              x_var <- get.var.as.numeric(o, vars, input$var_x)
              y_var <- get.var.as.numeric(o, vars, input$var_y)
              
              # delete unclear labels and title
              graphics::par(col.main="white", col.lab="white")
              tryCatch({
                dsBaseClient::ds.contourPlot(x = x_var,
                                             y = y_var,
                                             numints = input$intervals,
                                             show = "zoomed",
                                             datasources = o
                )
              },
              error=function(cond){
                errs <- DSI::datashield.errors()
                if( is.null(errs) )
                  stop(cond)
                else
                  stop(errs)
              },
              warning=function(cond){
                cat(paste0(Sys.time()," ", cond,'\n'), file=LOG_FILE, append=TRUE)
              })
              
              
              graphics::title(main = ifelse(input$title != "",
                                            input$title,
                                            paste("Correlation of", input$var_x, "and", input$var_y)),
                              col.main="black"
              )
              graphics::mtext(input$var_x, side=1, line=3, col = "black")
              graphics::mtext(input$var_y, side=2, line=3, col = "black")
              
            } else if ( input$plotType == "heatmap") {
              cat(paste0(Sys.time(),"  ","User ",globalValues$username," is analyzing ", input$var_x," against ", input$var_y," with an heatmap plot", "\n"), file=LOG_FILE, append=TRUE)
              y_var <- get.var.as.numeric(o, vars, input$var_y)
              x_var <- get.var.as.numeric(o, vars, input$var_x)
              
              graphics::par(col.main="white", col.lab="white")
              tryCatch({
                dsBaseClient::ds.heatmapPlot(x = x_var,
                                             y = y_var,
                                             numints = input$intervals,
                                             show = "zoomed",
                                             datasources = o
                )
              },
              error=function(cond){
                errs <- DSI::datashield.errors()
                if( is.null(errs) )
                  stop(cond)
                else
                  stop(errs)
              },
              warning=function(cond){
                cat(paste0(Sys.time()," ", cond,'\n'), file=LOG_FILE, append=TRUE)
              })
              
              graphics::title(main = ifelse(input$title != "",
                                            input$title,
                                            paste("Heatmap of", input$var_x, "and", input$var_y)),
                              col.main="black"
              )
              graphics::mtext(input$var_x, side=1, line=3, col = "black")
              graphics::mtext(input$var_y, side=2, line=3, col = "black")
              
            } else if ( input$plotType == "boxplot") {
              cat(paste0(Sys.time(),"  ","User ",globalValues$username," is performing box-plot on ", input$vars_x,"\n"), file=LOG_FILE, append=TRUE)
              get.vars.as.numeric(o, 'D', 'D.num', input$vars_x, vars);
              
              tryCatch({
                dsBaseClient::ds.boxPlot(datasources=o, x='D.num', variables=input$vars_x, type="pooled")
              },
              error=function(cond){
                errs <- DSI::datashield.errors()
                if( is.null(errs) )
                  stop(cond)
                else
                  stop(errs)
              },
              warning=function(cond){
                cat(paste0(Sys.time()," ", cond,'\n'), file=LOG_FILE, append=TRUE)
              })
            } else if ( input$plotType == "vim") {
              cat(paste0(Sys.time(),"  ","User ",globalValues$username," is performing NA imputation (VIM) on ", input$vars_x,"\n"), file=LOG_FILE, append=TRUE)
              get.vars.as.numeric(o, 'D', 'D.num', input$vars_x, vars);
              
              tryCatch({
                plots <- dsSwissKnifeClient::dssVIM('aggr',data='D.num', newobj = NULL, datasources=o)
                graphics::par(col.main="white", col.lab="white", mfrow = c(length(plots),1) )
                # we can plot the results of the aggr function for each node:
                for(p in plots){
                  graphics::plot(p)
                }
              },
              error=function(cond){
                errs <- DSI::datashield.errors()
                if( is.null(errs) )
                  stop(cond)
                else
                  stop(errs)
              },
              warning=function(cond){
                cat(paste0(Sys.time()," ", cond,'\n'), file=LOG_FILE, append=TRUE)
              })
              
            } else if ( input$plotType == "correlation") {
              cat(paste0(Sys.time(),"  ","User ",globalValues$username," is performing CCA on ", input$var_x," against ", input$var_y, "\n"), file=LOG_FILE, append=TRUE)
              get.vars.as.numeric(o, 'D', 'D.num', c(input$cca_vars_x, input$cca_vars_y), vars);

              tryCatch({
                res = dsCOVclient::dsrCCA(o, 'D.num', input$cca_vars_x, input$cca_vars_y, lambda1 = input$cca_lambda1, lambda2 = input$cca_lambda2)
                #res = dsrCCA(o, 'D.num', input$vars_x, input$vars_y, lambda1 = input$cca_lambda1, lambda2 = input$cca_lambda2)
                mixOmics::plotIndiv(res, ind.names = F,
                                    legend = F, title = 'Federated Correlation Analysis (dsrCCA)')
              },
              error=function(cond){
                errs <- DSI::datashield.errors()
                if( is.null(errs) )
                  stop(cond)
                else
                  stop(errs)
              },
              warning=function(cond){
                cat(paste0(Sys.time()," ", cond,'\n'), file=LOG_FILE, append=TRUE)
              })
            }else if(input$plotType == "princomp"){
              cat(paste0(Sys.time(),"  ","User ",globalValues$username," is performing Principal Component Analysis (PCA) on current table..\n"), file=LOG_FILE, append=TRUE)
              get.vars.as.numeric(o, 'D', 'Variables', 
                                  input$knn_vars_x,
                                  vars);
              tryCatch({
                princomp <- dsSwissKnifeClient::dssPrincomp(df='Variables', type="combine", 
                                                            center=T, scale=F,
                                                            scores.suffix='.scores',
                                                            async=T, datasources=o);
                knn <- dsSwissKnifeClient::dssKmeans('Variables', centers = input$knn_clusters, 
                                                     iter.max = input$knn_max_iter, nstart = input$knn_start, 
                                                     datasources=o);
                globalValues$last_KNN <- knn;
                dsSwissKnifeClient::biplot.dssPrincomp(princomp$global,
                                                       type="combine",
                                                       draw.arrows = T,
                                                       levels = 'Variables_km_clust3',
                                                       datasources=o);
              },
              error=function(cond){
                errs <- DSI::datashield.errors()
                if( is.null(errs) )
                  stop(cond)
                else
                  stop(errs)
              },
              warning=function(cond){
                cat(paste0(Sys.time()," ", cond,'\n'), file=LOG_FILE, append=TRUE)
              })
            }else if(input$plotType == "randomforest"){
              cat(paste0(Sys.time(),"  ","User ",globalValues$username," is performing Random Forest training on current table..\n"), file=LOG_FILE, append=TRUE)
              get.vars.as.numeric(o, 'D', 'D.num', c(input$var_y, input$vars), vars);
              tryCatch({
                rfs <- dsSwissKnifeClient::dssRandomForest(train=list(what='D.num', dep_var=input$var_y, expl_vars=input$vars, localImp=T),
                                                            async=F, datasources=o);
                globalValues$last_RFS <- rfs;
                #min_depth_frame <- randomForestExplainer::min_depth_distribution(rfs[[1]])
                #randomForestExplainer::plot_min_depth_distribution(min_depth_frame)
                importance_frame <- randomForestExplainer::measure_importance(rfs[[1]])
                randomForestExplainer::plot_multi_way_importance(importance_frame, 
                                                                 x_measure=input$x_measure, 
                                                                 y_measure=input$y_measure, 
                                                                 size_measure=input$size_measure)
              },
              error=function(cond){
                errs <- DSI::datashield.errors()
                if( is.null(errs) )
                  stop(cond)
                else
                  stop(errs)
              },
              warning=function(cond){
                cat(paste0(Sys.time()," ", cond,'\n'), file=LOG_FILE, append=TRUE)
              })
            }
          }
        })
        
      }) 
      
      # lm/glm formula
      output$modelFormula <- shiny::renderUI({
        # take dependency on button, execute something only if his "clicked" value is greater or equal to 1
        if(!globalValues$showAnalysis || !is.analysis.ready())
          return(NULL)
        
        if(input$analysis == 'lm' || input$analysis == 'glm'){
          family <- shiny::isolate({
            if( is.analysis.ready()
                && !is.null(input$var_explanatory) && input$var_explanatory != '' ){
              if( input$analysis == 'lm' )
                'gaussian'
              else
                input$familyFunction
            }
          })
        
          cat(paste0(Sys.time(),"  ","User ",globalValues$username," is building a linear regression model: ", paste0(input$var_target, ' ~ ', input$var_explanatory, "'", " family '", family, "' link '", available_glm_models[family],"'"), "\n"), file=LOG_FILE, append=TRUE)
          shiny::h4(paste0("Evaluated model '", paste0(input$var_target, ' ~ ', input$var_explanatory, "'", " family '", family, "' link '", available_glm_models[family],"'")))
        }else{
          cat(paste0(Sys.time(),"  ","User ",globalValues$username," is building a cox model: ", paste0(input$var_target, ' ~ ', input$var_explanatory, "'")), file=LOG_FILE, append=TRUE)
          shiny::h4(paste0("Evaluated cox model '", paste0(input$var_target, ' ~ ', input$var_explanatory, "'")))
        }
      })
      
      output$modelSummary <- shiny::renderUI({
        # take dependency on button, execute something only if his "clicked" value is greater or equal to 1
        if(!globalValues$showAnalysis || !is.analysis.ready())
          return(NULL)
        
        if(input$analysis == 'lm' || input$analysis == 'glm'){
          htm <- shiny::isolate({
            if( !is.null(input$var_explanatory) && input$var_explanatory != '' 
                && is.analysis.ready() ){
              tryCatch({
                #o <- get.ds.login()
                mod <- get.ds.glm.full.model()
                
                if( is.null(mod) )
                  return(shiny::HTML(paste0('<div style="color:red;">Error evaluating model: Did not converge after ', glm_max_iterations, ' iterations</div>')))
                
                nmod <- get.ds.glm.null.model()
                #smod <- summary(mod)
                # === NB THIS HOLDS ONLY FOR GLM WITH GAUSSIAN(IDENTITY) ===
                if( input$analysis == 'lm' ){
                  R2df <- get.R2.gaussian.glm(mod, nmod)
                  Fdf <- get.F.gaussian.glm(mod, nmod)
                  NullDev <- paste0("Null deviance: ", round(nmod$dev,2) , ' on ', nmod$df, " degrees of freedom<br/>")
                  Rstring <- paste0("<br/>Multiple R-squared:  ",round(R2df$R2[1],4),"	Adjusted R-squared:  ", round(R2df$R2adj[1],4), "<br/>")
                  Fstring <- paste0("F-statistic: ", round(Fdf$Fval,4), " on ", Fdf$Fdf1 ," and ", Fdf$Fdf2, " DF,  p-value: ", round(Fdf$Ftest,4))
                }else{
                  NullDev <- ""
                  Rstring <- ""
                  Fstring <- ""
                }
                shiny::HTML(paste0("<p>",
                                   NullDev,
                                   "Residual deviance: ", round(mod$dev,2) , ' on ', mod$df, " degrees of freedom",
                                   #            "AIC: ", round(smod$aic,4), 
                                   Rstring, 
                                   Fstring,
                                   "</p>"))
                
              }, error = function(e) {
                errs <- DSI::datashield.errors();
                if( is.null(errs) ){
                  shiny::HTML(paste0('<div style="color:red;">Error evaluating model: \'', errs ,'\'</div>'))
                  cat(paste0(Sys.time()," ", errs,'\n'), file=LOG_FILE, append=TRUE)
                }else{
                  shiny::HTML(paste0('<div style="color:red;">Error evaluating model: \'', e ,'\'</div>'))
                  cat(paste0(Sys.time()," ", e,'\n'), file=LOG_FILE, append=TRUE)
                }
              },
              warning=function(cond){
                shiny::HTML(paste0('<div style="color:red;">Error evaluating model: \'', cond ,'\'</div>'))
                cat(paste0(Sys.time()," ", cond,'\n'), file=LOG_FILE, append=TRUE)
              })
            }
          })
          return(htm)
        }else{
          return(shiny::plotOutput(ns("analysisPlot")))
        }
        
      })
      
      output$analysisPlot <- shiny::renderPlot({
        if(!globalValues$showAnalysis || !is.load.ready() || !is.analysis.ready())
          return(NULL)
        
        o <- get.ds.login()
        vars <- get.ds.project.table.variables()
        # do not take dependency on these objects
        shiny::isolate({
          if ( input$analysis == 'coxp' ) {
            input$var_time
            input$var_target
            input$var_explanatory
            get.vars.as.numeric(o, 'D', 'D.num', invars, vars);
            model_vars <- get.model.vars(o, vars, , , input$analysis, input$familyFunction)
            time_var <- get.var.as.numeric(o, vars, )
            tryCatch({
              output <- paste0("survival::Surv(",time_var,", ",model_vars$output,")")
              formula <- paste0(output," ~ ", model_vars$input)
              print(formula)
              cox.res <- dsSwissKnifeClient::dssCoxph(formula=formula,
                                                      data='D.num',
                                                      datasources=o)
              print(cox.res)
              plot(cox.res$server1$fit, conf.int = TRUE, col = c('blue', 'red'))
            },
            error=function(cond){
              errs <- DSI::datashield.errors()
              if( is.null(errs) )
                stop(cond)
              else
                stop(errs)
            },
            warning=function(cond){
              cat(paste0(Sys.time()," ", cond,'\n'), file=LOG_FILE, append=TRUE)
            })
          }
        })
      })
      
      # lm/glm coefficients
      output$modelCoefficients <- shiny::renderTable({
        # take dependency on button, execute something only if his "clicked" value is greater or equal to 1
        if(!globalValues$showAnalysis || input$analysis == 'coxp')
          return(NULL)
        
        tbl <- shiny::isolate({
          if( !is.null(input$var_explanatory) && input$var_explanatory != '' 
              && is.analysis.ready() ){
            tryCatch({
              mod <- get.ds.glm.full.model()
              coeff <- mod$coefficients
              cbind(Variables=row.names(coeff), round(coeff, 3))
            }, error = function(e) {
              errs <- DSI::datashield.errors()
              if( is.null(errs) )
                stop(e)
              else
                stop(errs)
            })
            
          }
          
        })
        
        return(tbl)
        
      })
      # === END ANALYSIS
   })
}



# === UTILS

get.dbnp.study.names <- function(studies) {
  if ( is.null(studies) )
    return("The server is not reachable")
  ret <- sapply(studies, function(x) x$token)
  names(ret) <- sapply(studies, function(x) x$title)
  if( length(ret) == 0)
    return( ret )
  else
    return( sort(ret, decreasing = T) )
}

dbnp.login <- function(login){
  PhenotypeDatabaseRClient::setPhenotypeDatabaseBaseUrl(login$url)
  PhenotypeDatabaseRClient::authenticate(login$user, login$password, login$skey)
}

get.dbnp.study.assay.names <- function(studyToken) {
  if ( is.na(studyToken) )
    return(NULL)
  assays <- PhenotypeDatabaseRClient::getAssaysForStudy(studyToken)
  if ( length(assays) == 0 )
    return(NULL)
  
  # filter out non-public assays
  # assays_public <- assays[sapply(assays, function(x) x$publicassay)]
  # assays <- assays_public
  ret <- sapply(assays, function(x) x$token)
  names(ret) = sapply(assays, function(x) x$name)
  if( length(ret) == 0)
    return( ret )
  else
    return( sort(ret, decreasing = T) )
}

doOpalLogin <- function(login) {
  o <- NULL
  
  for (i in 1:nrow(login)) {
    entry <- login[i, ]
    
    oo <- opalr::opal.login(username = entry$user,
                            password = entry$password,
                            url      = entry$url)
    
    if ( is.null(o) ) {
      o <- list(oo)
      names(o) <- c(entry$server)
    } else {
      o <- c(o, oo)
      names(o)[length(o)] <- entry$server
    }
  }
  
  return(o)
}

get.named.opal.projects <- function(o) {
  projects <- list()
  if ( class(o) == "list" ) {
    projects[[1]] <- opalr::opal.datasources(o[[1]])
  } else {
    projects[[1]] <- opalr::opal.datasources(o)
  }
  names(projects) <- names(o)
  # return a list of project names
  return(projects)
}

get.opal.projects <- function(o) {
  projects <- list()
  if ( class(o) == "list" ) {
    projects <- opalr::opal.datasources(o[[1]])
    # lapply(X = o, FUN = function(x) opal.datasources(x))
  } else {
    projects <- opalr::opal.datasources(o)
  }
  # return a list of project names
  return(projects)
}

get.var.as.factor <- function(o, vars, var){
  get.var.as(o, 'D',"factor", "_fac", vars, var)
}

get.var.as.numeric <- function(o, vars, var){
  get.var.as(o, 'D', c("numeric","integer"), "_num", vars, var)
}

# convert D table to D_num with all the columns of D converted to numeric
get.vars.as.numeric <- function(o, in_df, out_df, vars, in_df_var_types){
  get.vars.as(o, in_df, out_df, vars, in_df_var_types, c("numeric","integer"), "")
}

get.vars.as <- function(o, in_df, out_df, vars, in_df_var_types, target_types, target_suffix){
  vars_num<-sapply(vars, function(varname){
    get.var.as(o, in_df, target_types, target_suffix, in_df_var_types, varname)
  })
  dsBaseClient::ds.dataFrame(x=as.character(vars_num), 
                             #row.names = names(vars_num), 
                             stringsAsFactors = F,
                             newobj=out_df,
                             notify.of.progress = F,
                             datasources=o)
}

get.var.as <- function(o, in_df, target_types, target_suffix, in_df_var_types, var){
  output_var <- NULL
  all_types <- c("factor","character","integer","numeric")
  other_types <- all_types[sapply(all_types, function(x){all(x!=target_types)})]
  
  in_var <- paste0(in_df,"$",var)
  output_var <- paste0(var,target_suffix)
  
  if( any(as.character(in_df_var_types[var, ]) == other_types) ){
    # convert and assign
    if( any(target_types == "numeric") )
      tryCatch({
        dsBaseClient::ds.asNumeric(x.name=in_var, newobj=output_var, datasources=o)
      },
      error=function(cond){
        errs <- DSI::datashield.errors()
        if( is.null(errs) )
          stop(cond)
        else
          stop(errs)
      })
    else if( any(target_types == "factor") )
      tryCatch({
        dsBaseClient::ds.asFactor(input.var.name=in_var, newobj.name=output_var, datasources=o)
      },
      error=function(cond){
        errs <- DSI::datashield.errors()
        if( is.null(errs) )
          stop(cond)
        else
          stop(errs)
      })
    else
      stop(paste0("Error unknown target type ", target_types))
  }else{
    # just assign
    dsBaseClient::ds.assign(toAssign = in_var, newobj = output_var, datasources=o)
  }
  return(output_var)
}

get.model.vars <- function(o, vars, input, output, analysisType, familyFunction){
  # append D$ to each explanatory variable
  explanatory_var <- gsub("([a-zA-Z0-9#^_.]+)", "D$\\1", input)
  # re-sub D$1 with 1.. (for the intercept)
  explanatory_var <- gsub("D$1", "1", explanatory_var, fixed=TRUE)
  # find #<type> after the variables
  matches <- regmatches(explanatory_var, gregexpr("D\\$[a-zA-Z0-9^_.]+\\#[a-z]+", explanatory_var))
  matches <- unlist(matches)
  # -------- apply conversions ------
  sapply(matches, function(m){
    type <- substr(m, nchar(m)-2, nchar(m))
    name <- substr(m, 0, nchar(m)-4)
    no_dataframe_name <- substr(m, 3, nchar(m)-4)
    # apply conversions
    switch (type,
            'num'= { dsBaseClient::ds.asNumeric(x.name=name, newobj=paste0(no_dataframe_name,'_conv'), datasources=o) },
            'fac'={ dsBaseClient::ds.asFactor(input.var.name=name, newobj.name=paste0(no_dataframe_name,'_conv'), datasources=o) },
            {
              stop(paste0('casting to \'', type, '\' not valid, please use #fac for factorial and #num for numeric'))
            }
    )
  })
  
  # after creation of converted variables server side  
  # string replace the explanatory expression to use them
  explanatory_var <- gsub("D\\$([a-zA-Z0-9^_.]+)\\#[a-z]+", "\\1_conv", explanatory_var)
  
  output_var <- get.model.output.var(output, analysisType, familyFunction, vars, o)
  
  return(data.frame(output=output_var,input=explanatory_var))
}

get.model.output.var <- function(output, analysisType, familyFunction, vars, o){
  if((analysisType == 'lm' || analysisType == 'glm') && familyFunction == 'binomial'){
    output_var <- get.var.as.factor(o, vars, output)
  }else
    # if factorial, convert it to numeric otherwise we cannot apply family gaussian
    output_var <- get.var.as.numeric(o, vars, output)
  #output_var <- paste0("D$",output)
}

get.input.named.vars <- function(vars){
  varnames <- row.names(vars)
  vartypes <- vars$type
  varlabels <- paste0(varnames, ' [', vartypes, ']')
  names(varnames) <- varlabels
  varnames <- as.list(varnames)
  return(varnames)
}

# get R-squared for a gaussian glm with identity link
get.R2.gaussian.glm <- function(model, null_model){
  #p <- length(model$coefficients)-1
  p <- dim(model$coefficients)[1]
  N <- model$df + p
  R2<-1-(model$dev/null_model$dev)
  Rajust<-1-(((1-R2)*(N-1))/(N-p-1))
  return(data.frame(R2=R2,R2adj=Rajust))
}

# get F-test for a gaussian glm with identity link
get.F.gaussian.glm <- function(model, null_model){
  # explanatory variables (remove intercept)
  #p<-length(model$coefficients)
  p <- dim(model$coefficients)[1]
  df.diff <- null_model$df - model$df #p-1
  df.resid <- model$df #N-p
  Fval <- ((null_model$dev - model$dev)/df.diff)  /  ((model$dev)/df.resid)
  Ftest <- stats::pf(Fval,  df.diff, df.resid, lower.tail=FALSE)
  return(data.frame(Fval=Fval, Ftest=Ftest, Fdf1=df.diff, Fdf2=df.resid))
}

# === END UTILS

