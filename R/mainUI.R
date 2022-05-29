#' Build shiny UI page
#'
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets useShinydashboard
#' @importFrom shiny shinyUI fluidPage navbarPage tabPanel icon includeHTML br hr navbarMenu fluidRow strong p tags
#' @importFrom shinyalert useShinyalert
#' @importFrom shinydashboard box
#' @import shinyjs
#'
#' @export
#'
mainUI <- function() {

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

      # use_waiter(),
      # waiter_show_on_load(),
      # waiter_show_on_load(html = spin_ball()), # place at the top before content

      navbarPage(
        id = "mainMenu",
        title = "QRAP",
        theme = "www/style/style.css",
        # footer = includeHTML("footer.html"),
        fluid = TRUE,
        collapsible = TRUE,

        #""
        # ----------------------------------
        # tab panel 1 - Home
        tabPanel(
          "Home", value = "home", icon = icon("home"),
          # style = "margin-left:100px;",
          includeHTML(system.file("shiny",  "myApp/home.html", package = "QRAP")),
          source(system.file("shiny", "myApp/modules/ui-home-introduction.R", package = "QRAP"), local = T)$value,
          br(),hr()
        ),

        # ----------------------------------
        # tab panel 2 - Neighborhood Browser
        navbarMenu(
          title = "Data Preprocess",
          tabPanel("Get Start", value = "get_start", source(system.file("shiny", "myApp/modules/1-ui-get-start.R", package = "QRAP"), local = T)$value),
          tabPanel("Design & Run", value = "deseq", source(system.file("shiny", "myApp/modules/2-ui-condition.R", package = "QRAP"),local = T)$value)
        ),

        # ----------------------------------
        # tab panel 4 - About
        navbarMenu(
          title = "Quality Assessment",
          tabPanel("PCA", value = "pca", source(system.file("shiny", "myApp/modules/3-ui-pca.R", package = "QRAP"), local = T)$value),
          tabPanel("Hierarchical clustering", value = "hiera", source(system.file("shiny", "myApp/modules/4-ui-hierarchical-cluster.R", package = "QRAP"), local = T)$value),
          tabPanel("Sample to sample distance", value = "dis", source(system.file("shiny", "myApp/modules/5-ui-sample-distance.R", package = "QRAP"), local = T)$value),
          tabPanel("Sample correlation coefficient", value = "cor", source(system.file("shiny", "myApp/modules/6-ui-sample-correlation.R", package = "QRAP"), local = T)$value)
        ),

        navbarMenu(
          title = "Expression",
          tabPanel("Differential Expression Analysis", value = "dea", source(system.file("shiny", "myApp/modules/7-ui-differential-analysis.R", package = "QRAP"), local = T)$value),
          tabPanel("DEGs Expression Pattern", value = "degsp", source(system.file("shiny", "myApp/modules/8-ui-degs-patterns.R", package = "QRAP"), local = T)$value),
          tabPanel("Expression Visualization", value = "epv", source(system.file("shiny", "myApp/modules/9-ui-expression-visualization.R", package = "QRAP"), local = T)$value)
        ),

        navbarMenu(
          title = "WGCNA",
          tabPanel("1. Data Preparation", value = "wgcna-1", source(system.file("shiny", "myApp/modules/10-ui-wgcna-prepare-data.R", package = "QRAP"), local = T)$value),
          tabPanel("2. SoftThreshold Detection", value = "wgcna-2", source(system.file("shiny", "myApp/modules/CopyOf11-ui-wgcna-detect-module.R", package = "QRAP"), local = T)$value),
          tabPanel("3. Gene Module Detection", value = "wgcna-3", source(system.file("shiny", "myApp/modules/11-ui-wgcna-detect-module.R", package = "QRAP"), local = T)$value),
          tabPanel("4. Module-Traits Relationship", value = "wgcna-4", source(system.file("shiny", "myApp/modules/12-ui-wgcna-module-trait.R", package = "QRAP"), local = T)$value),
          tabPanel("5. Module membership vs. gene significanc", value = "wgcna-5", source(system.file("shiny", "myApp/modules/12.1-ui-wgcna-scatter.R", package = "QRAP"), local = T)$value),
          tabPanel("6. Module gene expression visualization", value = "wgcna-6", source(system.file("shiny", "myApp/modules/12.2-ui-wgcna-expression.R", package = "QRAP"), local = T)$value)
        ),

        navbarMenu(
          title = "Functions",
          tabPanel("ORA (gprofiler2)", value = "gprofiler2", source(system.file("shiny", "myApp/modules/13-ui-gProfiler.R", package = "QRAP"), local = T)$value),
          tabPanel("ORA (clusterProfiler)", value = "ORA_cluster", source(system.file("shiny", "myApp/modules/14-ui-cluster_ORA.R", package = "QRAP"), local = T)$value),
          tabPanel("GSEA (clusterProfiler)", value = "GSEA_cluster", source(system.file("shiny", "myApp/modules/15-ui-cluster_GSEA.R", package = "QRAP"), local = T)$value)

        ),

        navbarMenu(
          title = "Networks",
          tabPanel("KEGG Pathview (pathview)", value = "pathview", source(system.file("shiny", "myApp/modules/16-ui-kegg-pathview.R", package = "QRAP"), local = T)$value),
          tabPanel("Protein-Protein interaction (stringDB)", value = "ppi", source(system.file("shiny", "myApp/modules/17-ui-ppi-network.R", package = "QRAP"), local = T)$value),
          tabPanel("Inferred Regulation network (GENIE3)", value = "genie3", source(system.file("shiny", "myApp/modules/18-ui-genie3-network.R", package = "QRAP"), local = T)$value)
        ),

        navbarMenu(
          title = "Summarize",
          tabPanel("Summarize Genes", value = "sgene", source(system.file("shiny", "myApp/modules/19-ui-summarize-gene.R", package = "QRAP"), local = T)$value),
          tabPanel("Summarize Functions", value = "sfunc", source(system.file("shiny", "myApp/modules/20-ui-summarize-function.R", package = "QRAP"), local = T)$value)
        ),

        # ----------------------------------
        # tab panel last - About
        tabPanel(
          "FAQ",
          fluidRow(
            style = "margin-left: 10px; margin-right:10px;",
            p("Nothing .....")
          )
        )
      )
    )
  )
}
