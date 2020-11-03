fluidRow(
  style = "margin-left: 10px; margin-right:10px;",
  box(
    title = "blockwiseModules:", width = 4, collapsible = TRUE,
    switchInput("wgcna_cache", "Use Cache", value = F,
                onStatus = "success", offStatus = "danger", inline = T,labelWidth = "100px"),
    numericInput("maxBlockSize", "Maximum block size for module detection:", value = 5000, width = "100%"),
    numericInput("minModuleSize", "Minimum module size for module detection:", value = 30, width = "100%"),
    selectInput("blockwise_networkType", "networkType:", choices = c("unsigned", "signed", "signed hybrid"), width = "100%"),
    actionButton("blockwise_modal_but", "Additional Parameters for Normalization ...", width = "100%",
                 style = "background-color: rgb(255,255,255);text-align:left;margin-bottom:10px", icon = icon("plus-square")),
    br(),
    actionButton("moldue_detect", "Start moldue detect", width = "100%", class = "run-button")
  ),
  column(
    6,
    wellPanel(
      fluidRow(
        column(6, uiOutput("block_id")),
        column(
          6, align = "right", style = "padding-top: 10px;",
          dropdownButton(
            numericInput('plotDendro_width', 'Figure Width:', value = 10, width = "100%"),
            numericInput('plotDendro_height', 'Figure Height:', value = 5, width = "100%"),
            downloadButton('plotDendro_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
            circle = FALSE, status = "danger", size = "sm", icon = icon("save"), width = "200px",
            right = TRUE, tooltip = tooltipOptions(title = "Click to download figures !")
          )
        )
      ),
      uiOutput("wgcna_dendroUI")
    )
  ),
  column(
    2,
    wellPanel(
      sliderInput("wgcna_dendro_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
      sliderInput("wgcna_dendro_height", "Figure Height (px):", min = 200, max = 1000, value = 350, step = 20, width = "100%")
    )
  ),
  column(
    12,
    conditionalPanel(
      "input.moldue_detect",
      wellPanel(
        withSpinner(dataTableOutput("moduleGene_table")),
        downloadButton('moduleGene_table_csv','Download .csv', class = "btn", width = "100%")
      )
    )
  ),
  bsModal(
    "blockwise_modal", "Additional Parameters", "blockwise_modal_but", size = "large",
    column(
      12,
      style = "text-align:justify;color:black;background-color:lavender;border-radius:10px;border:1px solid black;", br(),
      h5("Additional Parameters of 'blockwiseModules':"), hr(),
      fluidRow(
        column(
          6,
          numericInput("blockSizePenaltyPower", "blockSizePenaltyPower:", value = 5, width = "100%"),
          numericInput("maxPOutliers", "maxPOutliers:", value = 1, width = "100%"),
          numericInput("quickCor", "quickCor:", value = 0, width = "100%"),
          numericInput("detectCutHeight", "detectCutHeight:", value = 0.995, width = "100%"),
          selectInput("impute", "impute:", choices = c("TRUE", "FALSE"), width = "100%"),
          selectInput("corType", "corType:", choices = c("pearson", "bicor"), width = "100%"),
          selectInput("TOMDenom", "TOMDenom:", choices = c("min", "mean"), width = "100%")
        ),
        column(
          6,
          numericInput("reassignThreshold", "reassignThreshold:", value = 1e-6, width = "100%"),
          numericInput("minCoreKME", "minCoreKME:", value = 0.5, width = "100%"),
          numericInput("minKMEtoStay", "minKMEtoStay:", value = 0.3, width = "100%"),
          numericInput("mergeCutHeight", "mergeCutHeight:", value = 0.15, width = "100%"),
          selectInput("TOMType", "TOMType:", width = "100%", selected = "signed",
                      choices = c("none", "unsigned", "signed", "signed Nowick", "unsigned 2", "signed 2", "signed Nowick 2")),
          selectInput("deepSplit", "deepSplit:", choices = c(0, 1, 2, 3, 4), selected = 2, width = "100%"),
          selectInput("pearsonFallback", "pearsonFallback:", choices = c("individual", "all", "none"), width = "100%")
        )
      )
    )
  ),
  column(
    12,
    hr(),
    fluidRow(column(2), column(3, actionLink("pWGCNA_2", "<< Previous", style = "font-size: 20px")),
             column(4, p("You are in WGCNA module detection page ...", style = "color: grey; font-size: 20px")),
             column(3, actionLink("nWGCNA_2", "Next >>", style = "font-size: 20px")))
  )
)
