fluidPage(
  style = "margin-left: 10px; margin-right:10px;",
  box(
    title = "Plot parameters:", width = 4, collapsible = TRUE,
    uiOutput("STRING_species"),
    numericInput("score_threshold", "Combined scores of the interactions:", value = 200, width = "100%"),
    uiOutput("ppi_group"),
    selectInput("ppi_genes", "Input Genes:", c("TopN Significant Genes" = "topN", "Selected DEGs" = "custom"), width = "100%"),
    conditionalPanel(
      "input.ppi_genes == 'topN'",
      numericInput("top_ppi_genes", "Top n most significant genes:", value = 200, width = "100%")
    ),
    conditionalPanel(
      "input.ppi_genes == 'custom'",
      uiOutput("ppi_degenes")
    ),
    p("*Please note: This step may take a long time if you are running in the first time,
      beacause some database need be download firstly.", style = "color = orange"),
    actionButton("plot_STRINGdb", "Start STRINGdb", class = "plot-button", width = "100%")
  ),
  column(
    6,
    wellPanel(
      dropdownButton(
        numericInput('ppi_width', 'Figure Width:', min = 1, max = 20, value = 10, width = "100%"),
        numericInput('ppi_height', 'Figure Height:', min = 1, max = 20, value = 10, width = "100%"),
        downloadButton('ppi_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
        circle = FALSE, status = "danger", size = "sm",
        icon = icon("save"), width = "200px",
        tooltip = tooltipOptions(title = "Click to download figures !")
      ),
      uiOutput("PPI_PlotUI")
    )
  ),
  column(
    2,
    wellPanel(
      sliderInput("ppi_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
      sliderInput("ppi_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 750, step = 20, width = "100%")
    )
  ),
  column(
    12,
    hr(),
    fluidRow(column(2), column(3, actionLink("pPPI", "<< Previous", style = "font-size: 20px")),
             column(4, p("You are in PPI notwork page ...", style = "color: grey; font-size: 20px")),
             column(3, actionLink("nPPI", "Next >>", style = "font-size: 20px")))
  )
)
