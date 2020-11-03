#   ____________________________________________________________________________
#   Server                                                                  ####

library(shiny)
library(plotly)
library(magrittr)
library(shinyjs)
library(stringr)
library(RColorBrewer)
library(DT)
library(shinyBS)
library(shinycssloaders)
library(STDAP)
library(shinyalert)
library(shinyWidgets) 
library(waiter)



shinyServer(function(session, input, output) {
  
  waiter_hide() # hide the waiter
  
  kegg_species <- reactive({
    readRDS("www/Species/kegg_species.rds")
  })
  
  observe({ kegg_species() })

  source("modules/1-server-get-start.R", local = T)
  source("modules/2-server-condition.R", local = T)
  source("modules/3-server-pca.R", local = T)
  source("modules/4-server-hierarchical-cluster.R", local = T)
  source("modules/5-server-sample-distance.R", local = T)
  source("modules/6-server-sample-correlation.R", local = T)
  source("modules/7-server-differential-analysis.R", local = T)
  source("modules/8-server-degs-patterns.R", local = T)
  source("modules/9-server-expression-visualization.R", local = T)
  source("modules/10-server-wgcna-prepare-data.R", local = T)
  source("modules/11-server-wgcna-detect-module.R", local = T)
  source("modules/12-server-wgcna-module-trait.R", local = T)
  source("modules/13-server-clusterProfiler.R", local = T)
  source("modules/14-server-gProfiler.R", local = T)
##  ............................................................................
##  Neighborhood browser                                                    ####
    
##  ............................................................................
##  Map chart                                                               ####

    
})
