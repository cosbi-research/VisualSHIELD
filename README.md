# VisualSHIELD

VisualSHIELD allows to create reactive visual web UIs to seamlessly analyze multiple remote datasets together,

hosted on [Opal](https://www.obiba.org/pages/products/opal/) through DataSHIELD.

The DASH-IN interactive federated analysis system, is a unifying visual federated analytical framework of observational and interventional studies that was the first incubator for the current VisualSHIELD.

VisualSHIELD development contribuited to the [ENPADASI](https://www.dtls.nl/wp-content/uploads/2016/05/ENPADASI_Bouwman_250516_FAIR.pdf#page=7) project of 51 partners in 9 European countries.

VisualSHIELD is a shiny app module offering ready-to-use self-contained UI and logic to be easily integrated in your custom Shiny App. 

See XXX SCREENSHOTS or XXX LIVE DEMO.

This repository contains the reference implementation for VisualSHIELD. You may freely use this work in your research and activities under the BSD 3-Clause License (see LICENSE file).

For more information and guided hands-on tutorials on how everything can be systematically glued together, as in the ENPADASI project, check out the [ENPADASI Hackaton](https://agenda.infn.it/event/11522/) 
or get in touch with the [Cosbi Bioinformatics lab](bioinformatics@cosbi.eu)

We'll be happy to help!

## Table of contents

- [Installation](#installation)
- [Self-hosting](#self-hosting)
  + [Automatic](#quick-dependencies)
  + [Manual](#manual-dependencies)
- [Usage](#usage)
  + [Embed the VisualSHIELD UI in your Shiny App](#embed-the-visualshield-server-module-in-your-shiny-app)
  + [Embed the VisualSHIELD Server in your Shiny App](#embed-visualshield-ui-module-in-your-shiny-app)

# Installation

On an R console just type
```
remotes::install_github('cosbi-research/VisualSHIELD@main', repos=c(getOption('repos'), 'http://cran.datashield.org', 'https://bioconductor.org/packages/3.3/bioc', 'https://bioconductor.org/packages/3.3/data/annotation','https://bioconductor.org/packages/3.3/data/experiment','https://bioconductor.org/packages/3.3/extra'))
```

If it fails, it may be because you don't have some headers for compiling the dependencies. 
On ubuntu, try to install these headers, and then try again:

```
sudo apt install libssl-dev libcurl4-openssl-dev libxml2-dev cmake
```

# Self Hosting

To quickly experiment with your own dataset, you can self-host your opal server together with DataSHIELD and all the required server-side dependencies.

This can be accomplished quickly with [docker compose](#quick-dependencies) or [manually](#manual-dependencies).

## Quick Dependencies

If you have Docker installed you can get up and running in a breeze with [docker compose](https://docs.docker.com/compose/).

Just cd into the VisualSHIELD directory and type

```
docker compose up -d
```

Docker will download and connect in the right way

- the mongodb datastore
- the opal server
- the rock R server with all the R dependencies pre-installed

When you finish using it you can turn off by cd into the VisualSHIELD directory and typing

```
docker compose stop
```

To re-start cd into the VisualSHIELD directory and type 

```
docker compose start
```

See [docker compose](https://docs.docker.com/compose/) documentation for further details.

## Manual Dependencies

see the [Manual installation manual](SETUP_OPAL.md) for instructions on how to manually install and set-up everything needed for performing remote experiments with VisualSHIELD.

# Usage

Make sure to have a (network of) OPAL server along with your secure credentials to integrate in the following analyses. For a demo, you can use https://opal-demo.obiba.org .

The VisualSHIELD module offers pre-built UI and logic for:
* analisis selection
* tabular result presentation
* ...
* XXXX

The analysis is performed through the privacy-aware [DataSHIELD](https://www.datashield.ac.uk/) analysis package, and allows to easily perform:
* histograms
* contour plots
* heatmaps
* linear models (lm)
* generalized linear models (glm)

VisualSHIELD module is exposed through the VisualSHIELDUI and the VisualSHIELDServer functions. 

To run the tutorial app go to the [example](example) folder and type

```R
> shiny::runApp()
```

An example output will be

```R
Loading required package: shiny

Listening on http://127.0.0.1:4079
Loading required package: httr
Loading required package: progress
Loading required package: R6
```

# Tutorial: Perform an individual level meta-analysis (ILMA) using a generalized linear model (GLM)

Perform the following steps to load your data into an opal server:
 * Access to your opal instance. If you don't have one, you can use the opal demo at obiba https://opal-demo.obiba.org (use administrator/password to login).
 * Upload on the opal server your data. You can also try our example data in the [data](example/data) folder.

Now go back to the VisualSHIELD instance. 
Once the app is running you can navigate to `http://127.0.0.1:4079` or any other link the shiny server is writing to the console.
The example app will require you to insert the name of one opal server to connect to, insert the link to the opal server you loaded your data into (ex. https://opal-demo.obiba.org ). 

Now click Update and the shiny module will be updated allowing you to select the data tables to analyze.
From "Opal Projects" select the name of your project, from "Data tables" select the name of the first of the tables you want to analyze.
Click "+" allows you to insert another project and another table, so that you can federate the analysis. 
**NOTE**: This is a single-server demo, but complex settings are not limited to a single opal server.

From now on we will assume you selected the example data tables Study.1 Study.2 Study.3 Study.4 Study.5 Study.6 .
Now click "Setup remote data tables"

## Embed VisualSHIELD UI module in your Shiny App

Embedding the UI in your custom shiny app is as simple as using VisualSHIELDUI in your page as any other shiny object. Below a simple
example of a minimal custom shiny UI that embeds VisualSHIELDUI.

```R
library(shiny)
library(opalr)
#> Loading required package: httr
library(DSI)
#> Loading required package: progress
#> Loading required package: R6
library(dsBaseClient)
library(VisualSHIELD)
shinyUI(
fluidPage(
    fluidRow(column(10, uiOutput("server")),
       column(2, actionButton("load", "Update"), tags$style(type='text/css',
          "#load { vertical-align: middle; margin-top: 25px;}"))),
    fluidRow(VisualSHIELDUI("VisualSHIELD", h3("Demo VisualSHIELD app")))
    )
)
```

Edit your `id`, and `title` as you like, just note that the choosen `id` should be the same as the one passed to the server module.

## Embed the VisualSHIELD Server module in your shiny app

Each shiny module also has a server counterpart for the UI. The VisualSHIELDServer communicates with the parent custom app through
the servers parameter, this means that we expect it to be a reactive block, returning a list (more on this below) or NULL.
Here is an example on how to embed it in your custom shinyServer function

```R
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
            # dbNP server whose studies will be migrated
            # to the opal server defined above
            dashin_server = NULL
        )
        #, ... server n
      )
    )
  )
})

VisualSHIELDServer("VisualSHIELD", servers=login)

output$server <- renderUI({
  textInput("custom_server",
            label="Server to connect to:",
            value="",
            placeholder = "https://opal-demo.obiba.org")
  })
})
```

You may always access these information from your preferred R coding environment with:

```R
vignette('VisualSHIELD')
```
