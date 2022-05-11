#' create the mainly server functions
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @importFrom shiny reactiveValues
#' @importFrom shinyalert shinyalert
#' @importFrom GEOquery getGEOSuppFiles getGEO
#' @importFrom DT renderDataTable
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyBS bsAlert createAlert closeAlert
#' @importFrom shinyWidgets pickerInput
#' @importFrom stringr str_remove
#' @importFrom plyr join_all
#' @importFrom utils head untar write.csv
#' @importFrom stats p.adjust
#' @importFrom enrichplot cnetplot emapplot
#'
#' @export

mainServer <- function(input, output, session) {
  source(system.file("shiny", "myApp/modules/1-server-get-start.R", package = "QRseq"), local = T)
  source(system.file("shiny", "myApp/modules/2-server-condition.R", package = "QRseq"), local = T)
  source(system.file("shiny", "myApp/modules/3-server-pca.R", package = "QRseq"), local = T)
  source(system.file("shiny", "myApp/modules/4-server-hierarchical-cluster.R", package = "QRseq"), local = T)
  source(system.file("shiny", "myApp/modules/5-server-sample-distance.R", package = "QRseq"), local = T)
  source(system.file("shiny", "myApp/modules/6-server-sample-correlation.R", package = "QRseq"), local = T)
  source(system.file("shiny", "myApp/modules/7-server-differential-analysis.R", package = "QRseq"), local = T)
  source(system.file("shiny", "myApp/modules/8-server-degs-patterns.R", package = "QRseq"), local = T)
  source(system.file("shiny", "myApp/modules/9-server-expression-visualization.R", package = "QRseq"), local = T)
  source(system.file("shiny", "myApp/modules/10-server-wgcna-prepare-data.R", package = "QRseq"), local = T)
  source(system.file("shiny", "myApp/modules/11-server-wgcna-detect-module.R", package = "QRseq"), local = T)
  source(system.file("shiny", "myApp/modules/CopyOf11-server-wgcna-detect-module.R", package = "QRseq"), local = T)
  source(system.file("shiny", "myApp/modules/12-server-wgcna-module-trait.R", package = "QRseq"), local = T)
  source(system.file("shiny", "myApp/modules/12.1-server-wgcna-scatter.R", package = "QRseq"), local = T)
  source(system.file("shiny", "myApp/modules/12.2-server-wgcna-expression.R", package = "QRseq"), local = T)
  source(system.file("shiny", "myApp/modules/13-server-gProfiler.R", package = "QRseq"), local = T)
  source(system.file("shiny", "myApp/modules/14-server-cluster_ORA.R", package = "QRseq"), local = T)
  source(system.file("shiny", "myApp/modules/15-server-cluster_GSEA.R", package = "QRseq"), local = T)
  source(system.file("shiny", "myApp/modules/16-server-kegg-pathview.R", package = "QRseq"), local = T)
  source(system.file("shiny", "myApp/modules/17-server-ppi-network.R", package = "QRseq"), local = T)
  source(system.file("shiny", "myApp/modules/18-server-genie3-network.R", package = "QRseq"), local = T)
  source(system.file("shiny", "myApp/modules/19-server-summarize-gene.R", package = "QRseq"), local = T)
  source(system.file("shiny", "myApp/modules/20-server-summarize-function.R", package = "QRseq"), local = T)
}
