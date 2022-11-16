fluidRow(
  style = "margin-left: 10px; margin-right:10px;",
  box(
    title = "SoftThreshold:", width = 4, collapsible = TRUE, solidHeader = TRUE,
    numericInput("power_RsquaredCut", "desired minimum scale free topology fitting index R^2:", value = 0.85, width = "100%"),
    # numericInput("power_BlockSize", "block size:", value = 1000, width = "100%"),
    numericInput("power_nBreaks", "number of bins in connectivity histograms:", value = 10, width = "100%"),
    selectInput("power_networkType", "networkType:", choices = c("unsigned", "signed", "signed hybrid"), width = "100%"),
    selectInput("power_corFnc", "corFnc:", choices = c("cor", "bicor"), width = "100%"),
    selectInput("moreNetworkConcepts", "moreNetworkConcepts:", choices = c("TRUE", "FALSE"), selected = "FALSE", width = "100%"),
    br(),
    actionButton("cal_power", "calculate SoftThreshold", width = "100%", class = "run-button")
  ),
  column(
    6,
    wellPanel(
      style = "padding-top:5px",
      fluidRow(
        column(
          12, style = "padding-left:0px;margin-left:0px;padding-right:0px;margin-right:0px;border-bottom:solid 1px rgb(224,224,224)",
          column(
            6, style = "padding-left:10px;",
            tags$h4("Soft Threshold:")
          ),
          column(
            6, align = "right", style = "padding-top:5px;",
            dropdownButton(
              numericInput('SoftThreshold_width', 'Figure Width:', value = 10, width = "100%"),
              numericInput('SoftThreshold_height', 'Figure Height:', value = 5, width = "100%"),
              downloadButton('SoftThreshold_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
              circle = FALSE, status = "danger", size = "sm", icon = icon("save"), width = "200px",
              right = TRUE, tooltip = tooltipOptions(title = "Click to download figures !")
            )
          )
        )
      ),
      uiOutput("wgcna_SoftThresholdUI")
    )
  ),
  column(
    2,
    wellPanel(
      sliderInput("wgcna_power_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
      sliderInput("wgcna_power_height", "Figure Height (px):", min = 200, max = 1000, value = 428, step = 2, width = "100%")
    )
  ),
  column(
    12, style = "padding:0px;",
    fluidRow(
      style = "background-color: rgb(248,249,250); border: 1px solid rgb(218,219,220); padding: 5px; margin:5px; border-radius: 15px;",
      column(
        4, style = "text-align:center;border-right: 2px solid white; padding-top:15px",
        tags$img(src = "images/demo/wgcna_power.png",
                 width = "100%")
      ),
      column(
        8, style = "text-align:justify;",
        h3("What is the Soft Threshold of WGCNA ?"),
        p("Constructing a weighted gene network entails the choice of the soft thresholding power β to which co-expression
          similarity is raised to calculate adjacency. The authors (B. Zhang and S. Horvath, 2005) have proposed to choose the soft thresholding 
          power based on the criterion of approximate scale-free topology. We refer the reader to that work for more details; 
          here we illustrate the use of the function pickSoftThreshold that performs the analysis of network topology and aids 
          the user in choosing a proper soft-thresholding power. The user chooses a set of candidate powers (the function provides
          suitable default values), and the function returns a set of network indices that should be inspected, We choose the power 
          which is the lowest power for which the scale-free topology fit index curve flattens out upon reaching a high value.")
        # h3("How to interpret the SSD analysis results ?"),
        # p("SSDA can elucidate samples distance in the high-dimensional space. In RNA-seq data, each gene is a dimension,
        #   so the data has tens of thousands of dimensions. SSDA uses Euclidean distance to elucidate samples distance in the
        #   high-dimensional space, which helps to understand the relationship of samples across exprimental conditions or sample replicates.
        #   The heatmap clusters samples with similar distances, which makes the results easier to interpret.")
      )
    )
  ),
  column(
    12,
    hr(),
    fluidRow(
      style = "margin-bottom:20px",
      column(3, align = "right", actionLink("pWGCNA_2", "<< Previous", style = "font-size: 20px")),
      column(6, align = "center"),
      column(3, align = "left", actionLink("nWGCNA_2", "Next >>", style = "font-size: 20px"))
    )
  )
)
