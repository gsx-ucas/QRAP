#   ____________________________________________________________________________
#   UI                                                                      ####
library(shiny)
library(plotly)
library(shinyjs)
library(shinyBS)
library(shinythemes)
library(shinyalert)
library(shinyWidgets) 
library(shinydashboard)
library(waiter)

jsCode <- jsCode <- "shinyjs.collapse = function(boxid) {
      $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
};"


shinyUI(
  fluidPage(
    style = "width:100%; padding: 0px",
    
    useShinyalert(),
    useShinydashboard(),
    
    useShinyjs(),
    extendShinyjs(text = jsCode, functions = "collapse"),
    
    use_waiter(),
    waiter_show_on_load(html = spin_ball()), # place at the top before content
    
    navbarPage(
      id = "mainMenu",
      title = "QRSeq",
      theme = "style/style.css",
      # footer = includeHTML("footer.html"),
      fluid = TRUE, 
      collapsible = TRUE,
      
      # 
      # ----------------------------------
      # tab panel 1 - Home
      tabPanel(
        "Home", value = "home", icon = icon("home"),
        # style = "margin-left:100px;",
        includeHTML("home.html"),
        source("modules/ui-home-introduction.R", local = T)$value,
        br(),hr()
      ),
      
      # ----------------------------------
      # tab panel 2 - Neighborhood Browser
      tabPanel("Get Start", value = "get_start",
               source("modules/1-ui-get-start.R", local = T)$value
      ),
      
      # ----------------------------------
      # tab panel 3 - Location Comparison
      tabPanel("Design & Run", value = "deseq",
               source("modules/2-ui-condition.R", local = T)$value
      ),
      
      # ----------------------------------
      # tab panel 4 - About
      navbarMenu(
        title = "Quality Assessment",
        tabPanel("PCA", value = "pca", source("modules/3-ui-pca.R", local = T)$value),
        tabPanel("Hierarchical clustering", value = "hiera", source("modules/4-ui-hierarchical-cluster.R", local = T)$value),
        tabPanel("Sample to sample distance", value = "dis", source("modules/5-ui-sample-distance.R", local = T)$value),
        tabPanel("Sample correlation coefficient", value = "cor", source("modules/6-ui-sample-correlation.R", local = T)$value)
      ),
      
      navbarMenu(
        title = "Expression Analysis",
        tabPanel("Differential Expression Analysis", value = "dea", source("modules/7-ui-differential-analysis.R", local = T)$value),
        tabPanel("DEGs Expression Pattern", value = "degsp", source("modules/8-ui-degs-patterns.R", local = T)$value),
        tabPanel("Expression Visualization", value = "epv", source("modules/9-ui-expression-visualization.R", local = T)$value)
      ),
      
      navbarMenu(
        title = "WGCNA",
        tabPanel("1. Creat Data", value = "wgcna-1", source("modules/10-ui-wgcna-prepare-data.R", local = T)$value),
        tabPanel("2. Module Detection", value = "wgcna-2", source("modules/11-ui-wgcna-detect-module.R", local = T)$value),
        tabPanel("3. Module-Traits Relationship", value = "wgcna-3", source("modules/12-ui-wgcna-module-trait.R", local = T)$value)
      ),
      
      navbarMenu(
        title = "Functional Analysis",
        # tabPanel("ORA analysis"),
        # tabPanel("GSEA analysis"),
        tabPanel("ORA & GSEA", value = "clusterProfiler", source("modules/13-ui-clusterProfiler.R", local = T)$value),
        tabPanel("g:Profiler API", value = "gProfiler", source("modules/14-ui-gProfiler.R", local = T)$value)
      ),
      
      navbarMenu(
        title = "Network Analysis",
        tabPanel("stringDB (PPI)"),
        tabPanel("GENIE3 (GRN)")
      ),
      
      # ----------------------------------
      # tab panel last - About
      tabPanel("About"
      )         
    )  
  )
)