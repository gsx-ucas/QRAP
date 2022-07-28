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
  source(system.file("shiny", "modules/1-server-get-start.R", package = "QRAP"), local = T)
  source(system.file("shiny", "modules/2-server-condition.R", package = "QRAP"), local = T)
  source(system.file("shiny", "modules/3-server-pca.R", package = "QRAP"), local = T)
  source(system.file("shiny", "modules/4-server-hierarchical-cluster.R", package = "QRAP"), local = T)
  source(system.file("shiny", "modules/5-server-sample-distance.R", package = "QRAP"), local = T)
  source(system.file("shiny", "modules/6-server-sample-correlation.R", package = "QRAP"), local = T)
  source(system.file("shiny", "modules/7-server-differential-analysis.R", package = "QRAP"), local = T)
  source(system.file("shiny", "modules/8-server-degs-patterns.R", package = "QRAP"), local = T)
  source(system.file("shiny", "modules/9-server-expression-visualization.R", package = "QRAP"), local = T)
  source(system.file("shiny", "modules/10-server-wgcna-prepare-data.R", package = "QRAP"), local = T)
  source(system.file("shiny", "modules/11-server-wgcna-detect-module.R", package = "QRAP"), local = T)
  source(system.file("shiny", "modules/CopyOf11-server-wgcna-detect-module.R", package = "QRAP"), local = T)
  source(system.file("shiny", "modules/12-server-wgcna-module-trait.R", package = "QRAP"), local = T)
  source(system.file("shiny", "modules/12.1-server-wgcna-scatter.R", package = "QRAP"), local = T)
  source(system.file("shiny", "modules/12.2-server-wgcna-expression.R", package = "QRAP"), local = T)
  source(system.file("shiny", "modules/13-server-gProfiler.R", package = "QRAP"), local = T)
  source(system.file("shiny", "modules/14-server-cluster_ORA.R", package = "QRAP"), local = T)
  source(system.file("shiny", "modules/15-server-cluster_GSEA.R", package = "QRAP"), local = T)
  source(system.file("shiny", "modules/16-server-kegg-pathview.R", package = "QRAP"), local = T)
  source(system.file("shiny", "modules/17-server-ppi-network.R", package = "QRAP"), local = T)
  source(system.file("shiny", "modules/18-server-genie3-network.R", package = "QRAP"), local = T)
  source(system.file("shiny", "modules/19-server-summarize-gene.R", package = "QRAP"), local = T)
  source(system.file("shiny", "modules/20-server-summarize-function.R", package = "QRAP"), local = T)
}
