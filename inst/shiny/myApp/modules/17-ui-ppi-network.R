fluidPage(
  style = "margin-left: 10px; margin-right:10px;",
  column(
    4,
    box(
      title = "Run parameters:", id = "run_ppi_card", width = 12, collapsible = TRUE, solidHeader = TRUE,
      radioButtons("ppi_genes", "PPI Genes Source:", c("DEGs", "DEG Patterns", "WGCNA Modules"), inline = T, width = "100%"),
      uiOutput("ppi_group"),
      uiOutput("ppi_subGene"),
      uiOutput("required_score"),
      # numericInput("required_score", "Threshold of significance to include an interaction:", value = 400, width = "100%"),
      selectInput("network_type", "Network type:", c("functional", "physical"), width = "100%"),
      selectInput("hide_disconnected_nodes", "Hides all proteins that are not connected:", c("TRUE"='1', "FALSE"='0'), width = "100%"),
      selectInput("show_query_node_labels", "Use submitted names as protein labels:", c("TRUE"='1', "FALSE"='0'), width = "100%"),
      selectInput("block_structure_pics_in_bubbles", "Disables structure pictures inside the bubble:", c("TRUE"='1', "FALSE"='0'), width = "100%"),
      p("We using STRING API to perform this analysis, you can also go to the ",
        tags$a("STRING web page", href = "https://string-db.org/cgi/input?sessionId=inputgtsessionId&input_page_active_form=multiple_identifiers",
               target = "_blank"), "to perform this analysis.", style = "text-align:justify;padding:5px"),
      actionButton("Init_STRINGdb", "Start STRINGdb", class = "run-button", width = "100%")
    )
  ),
  column(
    6,
    # wellPanel(
    # uiOutput("PPI_iframe"),
    # hr(),
    withSpinner(uiOutput("PPI_Image"))
    # )
  ),
  column(
    2,
    wellPanel(
      sliderInput("ppi_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
      sliderInput("ppi_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 660, step = 20, width = "100%"),
      withSpinner(uiOutput("PPI_weblink"))
    )
  ),
  column(
    12,
    hr(),
    fluidRow(column(3, align = "right", actionLink("pPPI", "<< Previous", style = "font-size: 20px")),
             column(6, align = "center", HTML('<p style = "text-align:center;">Copyright &copy; 2022.Shixue All rights reserved.</p>')),
             column(3, align = "left", actionLink("nPPI", "Next >>", style = "font-size: 20px")))
  )
)
