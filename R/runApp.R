#' Start the QRseq shiny App
#'
#' @importFrom dplyr %>%
#' @importFrom shiny shinyApp runApp addResourcePath
#' @export
#'

startQRseq <- function() {

  # set upload file size limit as 100MB
  options(shiny.maxRequestSize = 1000 * 1024^2, warn = -1, shiny.sanitize.errors = TRUE)

  source(system.file("shiny", "myApp/global.R", package = "QRseq"), local = T)
  addResourcePath(prefix = "www", directoryPath = system.file("shiny", "myApp/www", package = "QRseq"))
  addResourcePath(prefix = "images", directoryPath = system.file("shiny", "myApp/www/images", package = "QRseq"))
  addResourcePath(prefix = "Kegg_dir", directoryPath = system.file("shiny", "myApp/www/Kegg_dir", package = "QRseq"))

  kegg_dir <- system.file("shiny", "myApp/www/Kegg_dir", package = "QRseq")
  lapply(dir(kegg_dir, full.names = TRUE), function(x){file.remove(x)})

  shinyApp(ui = mainUI, server = mainServer) %>% runApp(launch.browser = TRUE)

  # appDir <- system.file("shiny", "myApp", package = "QRseq")
  #
  # if (appDir == "") {
  #   stop("Could not find App directory. Try re-installing `QRseq`.", call. = FALSE)
  # }
  #
  # shiny::runApp(appDir, display.mode = "normal")
}
