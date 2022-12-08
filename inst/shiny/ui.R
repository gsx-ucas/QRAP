#   ____________________________________________________________________________
#   UI                                                                      ####
library(DT)
library(shiny)
library(plotly)
library(shinyjs)
library(shinyBS)
# library(shinythemes)
library(shinyalert)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)

jsCode <- "shinyjs.collapse = function(boxid) {
      $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
};"

# jsCode2 <- "shinyjs.fig_zoom = function() {
#   var modal = document.getElementById('myModal');
#   
#   var img = document.getElementById('myImg');
#   var modalImg = document.getElementById('caption');
#   var captionText = document.getElementById('caption);
#   img.onclick = function(){
#     modal.style.display = 'block';
#     modalImg.src = this.src;
#     captionText.innerHTML = this.alt;
#   }
#   
#   var span = document.getElementsByClassName('close')[0];
#   
#   span.onclick = function() { 
#     modal.style.display = 'none';
#   }
# }"


shinyUI(
  fluidPage(
    style = "width:100%; padding: 0px",

    useShinydashboard(),

    useShinyjs(),
    extendShinyjs(text = jsCode, functions = "collapse"),
    # extendShinyjs(text = jsCode2, functions = "fig_zoom"),

    tags$head(HTML("<title>QRAP</title>")),
    navbarPage(
      id = "mainMenu",
      # title = "QRAP",
      title = img(src = "images/logo_img.png", height = "30px", style = "margin-top: -2px"),
      theme = "style/style.css",
      # theme = "style/layui.css",
      # footer = includeHTML("footer.html"),
      fluid = TRUE,
      collapsible = TRUE,
      # footer = fluidRow(
      #   column(
      #     12, class = "footer", style = "position:relative;",
      #     HTML('<p style = "text-align:center;">Copyright &copy; 2022.Shixue All rights reserved.</p>')
      #   )
      # ),

      #
      # ----------------------------------
      # tab panel 1 - Home
      tabPanel(
        "Home", value = "home", icon = icon("home"),
        includeHTML("home.html"),
        source("modules/ui-home-introduction.R", local = T)$value,
        fluidRow(
          column(
            12, class = "footer", style = "position:relative;",
            HTML('<p style = "text-align:center;">Copyright &copy; 2022.Shixue All rights reserved.</p>')
          )
        )
        # column(
        #   12, style = "margin-left:0px;margin-right:0px;",
        #   hr(),
        #   HTML(
        #     paste0(
        #       '<p style = "text-align:center;">Copyright &copy; 2022.Shixue All rights reserved.</p>'
        #     )
        #   )
        # )
      ),

      # ----------------------------------
      # tab panel 2 - Neighborhood Browser
      navbarMenu(
        title = "Preprocess",
        tabPanel("Get Start", value = "get_start", source("modules/1-ui-get-start.R", local = T)$value),
        tabPanel("Design & Run", value = "deseq", source("modules/2-ui-condition.R", local = T)$value)
      ),

      # ----------------------------------
      # tab panel 4 - About
      navbarMenu(
        title = "Quality",
        tabPanel("PCA", value = "pca", source("modules/3-ui-pca.R", local = T)$value),
        tabPanel("Hierarchical clustering", value = "hiera", source("modules/4-ui-hierarchical-cluster.R", local = T)$value),
        tabPanel("Sample to sample distance", value = "dis", source("modules/5-ui-sample-distance.R", local = T)$value),
        tabPanel("Sample correlation coefficient", value = "cor", source("modules/6-ui-sample-correlation.R", local = T)$value)
      ),

      navbarMenu(
        title = "Expression",
        tabPanel("Differential Expression Analysis", value = "dea", source("modules/7-ui-differential-analysis.R", local = T)$value),
        tabPanel("DEGs Expression Pattern", value = "degsp", source("modules/8-ui-degs-patterns.R", local = T)$value),
        tabPanel("Expression Visualization", value = "epv", source("modules/9-ui-expression-visualization.R", local = T)$value)
      ),

      navbarMenu(
        title = "WGCNA",
        tabPanel("1. Creat Data", value = "wgcna-1", source("modules/10-ui-wgcna-prepare-data.R", local = T)$value),
        tabPanel("2. Power Detection", value = "wgcna-2", source("modules/CopyOf11-ui-wgcna-detect-module.R", local = T)$value),
        tabPanel("3. Module Detection", value = "wgcna-3", source("modules/11-ui-wgcna-detect-module.R", local = T)$value),
        tabPanel("4. Module-Traits Relationship", value = "wgcna-4", source("modules/12-ui-wgcna-module-trait.R", local = T)$value),
        tabPanel("5. Module membership vs. gene significanc", value = "wgcna-5", source("modules/12.1-ui-wgcna-scatter.R", local = T)$value),
        tabPanel("6. Module gene expression visualization", value = "wgcna-6", source("modules/12.2-ui-wgcna-expression.R", local = T)$value)
      ),

      navbarMenu(
        title = "Function",
        tabPanel("ORA (gprofiler2)", value = "gprofiler2", source("modules/13-ui-gProfiler.R", local = T)$value),
        tabPanel("ORA (clusterProfiler)", value = "ORA_cluster", source("modules/14-ui-cluster_ORA.R", local = T)$value),
        tabPanel("GSEA (clusterProfiler)", value = "GSEA_cluster", source("modules/15-ui-cluster_GSEA.R", local = T)$value)
      ),

      navbarMenu(
        title = "Network",
        tabPanel("KEGG Pathview (pathview)", value = "gprofiler2", source("modules/16-ui-kegg-pathview.R", local = T)$value),
        tabPanel("Protein-Protein interaction (stringDB)", value = "ppi", source("modules/17-ui-ppi-network.R", local = T)$value),
        tabPanel("Inferred Regulation network (GENIE3)", value = "genie3", source("modules/18-ui-genie3-network.R", local = T)$value)
      ),
      navbarMenu(
        title = "Summarize",
        tabPanel("Summarize Genes", value = "sgene", source("modules/19-ui-summarize-gene.R", local = T)$value),
        tabPanel("Summarize Functions", value = "sfunc", source("modules/20-ui-summarize-function.R", local = T)$value)
        # tabPanel("Summarize Function", value = "sfunc", source(system.file("shiny", "myApp/modules/15-ui-ppi-network.R", package = "QRseq"), local = T)$value),
      ),
      # ----------------------------------
      # tab panel last - About
      tabPanel("About"
      )
    )
  )
)
