# VisualSHIELD

<img align="right" height="100" src="https://dashin.cosbi.eu/img/dash-in_logo.png">

VisualSHIELD allows to create reactive visual web UIs to seamlessly analyze multiple remote datasets in parallel hosted on [Opal](https://www.obiba.org/pages/products/opal/) and optionally provides also a facility to load [dbNP](https://dashin.eu/interventionstudies/) data into Opal. The [DASH-IN interactive federated analysis system](https://dashin.cosbi.eu/) is a unifying visual federated analytical framework of observational and interventional studies powered by VisualSHIELD and contribuited to the [ENPADASI](https://www.dtls.nl/wp-content/uploads/2016/05/ENPADASI_Bouwman_250516_FAIR.pdf#page=7) project of 51 partners in 9 European countries.

This repository contains the reference implementation for VisualSHIELD. You may freely use this work in your research and activities under the non-commercial [COSBI-SSLA license](https://www.cosbi.eu/research/prototypes/licence_terms).

For more information and guided hands-on tutorials on how everything can be systematically glued together, as in the ENPADASI project, check out the [ENPADASI Hackaton](https://agenda.infn.it/event/11522/) or get in touch with the Cosbi Bioinformatics lab, led by lombardo@cosbi.eu. We'll be happy to help!

## Table of contents

- [Installation](#installation)
- [Usage](#usage)


# Installation

XXXX PRE-requisites prior to install.packages()? 


Finally, from R console type

```R
install.packages('VisualSHIELD_1.0.tar.gz')
```

For REVIEWERS, type:

XXXXXXX

# Usage

For a demo of what you can obtain out-of-box, after installation go to the [example](example) folder, at the R console type:

```R
shiny::runApp() 
```

VisualSHIELD is a shiny app module. A shiny app module is a self-contained UI with it's own logic that can be easily integrated in any other custom shiny app. 

The module offers pre-built UI and logic for:
* analisis selection
* tabular resutl presentation
* ...
* XXXX

The analysis is performed through the privacy-aware [DataSHIELD](https://www.datashield.ac.uk/) analysis package, and allows to easily perform:
* histograms
* contour plots
* heatmaps
* linear models (lm)
* generalized linear models (glm)

VisualSHIELD module is exposed through the VisualSHIELDUI and the VisualSHIELDServer functions. 


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

## Embed the Server module in your shiny app

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

You may alwasy access these information from your preferred R coding environment with:

```R
vignette('VisualSHIELD')
```
