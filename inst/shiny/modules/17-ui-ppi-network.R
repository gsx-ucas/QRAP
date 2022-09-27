fluidPage(
  style = "margin-left: 10px; margin-right:10px;",
  column(
    4,
    box(
      title = "Run parameters:", id = "run_ppi_card", width = 12, collapsible = TRUE, solidHeader = TRUE,
      radioButtons("ppi_genes", "PPI Genes Source:", c("DEGs", "DEG Patterns", "WGCNA Modules"), inline = T, width = "100%"),
      uiOutput("ppi_group"),
      # uiOutput("ppi_subGene"),
      # uiOutput("required_score"),
      numericInput("required_score", "Threshold of significance to include an interaction:", value = 400, width = "100%"),
      selectInput("network_type", "Network type:", c("functional", "physical"), width = "100%"),
      selectInput("hide_disconnected_nodes", "Hides all proteins that are not connected:", c("TRUE"='1', "FALSE"='0'), width = "100%"),
      selectInput("show_query_node_labels", "Use submitted names as protein labels:", c("TRUE"='1', "FALSE"='0'), width = "100%"),
      selectInput("block_structure_pics_in_bubbles", "Disables structure pictures inside the bubble:", c("TRUE"='1', "FALSE"='0'), width = "100%"),
      actionButton("Init_STRINGdb", "Start STRINGdb", class = "run-button", width = "100%")
    )
  ),
  column(
    6,
    wellPanel(
      style = "border: 1px solid rgb(224,224,224);background-color: white;",
      withSpinner(htmlOutput("PPI_Image"))
    )
  ),
  column(
    2,
    wellPanel(
      sliderInput("ppi_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
      sliderInput("ppi_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 570, step = 20, width = "100%"),
      withSpinner(uiOutput("PPI_weblink"))
    )
  ),
  column(
    12,
    fluidRow(
      style = "background-color: rgb(248,249,250); border: 1px solid rgb(218,219,220); padding: 5px; margin:5px; border-radius: 15px;",
      column(
        4, style = "text-align:center;border-right: 2px solid white;",
        tags$img(src = "images/dist_demo.png",
                 width = "100%")
      ),
      column(
        8, style = "text-align:justify;",
        h3("What is sample-to-sample distance (SSD) ?"),
        p("Sample-to-sample distance (SSD) is another method to assess sequencing and sample replicates
          quality based on calculated distance between samples. SSDA calculated similarity between samples based on
          distance metrics, which specify how the distance between the input samples. A commonly used approach for
          measuring sample distance in RNA-seq data is to use Euclidean distance."),
        h3("How to interpret the SSD analysis results ?"),
        p("SSDA can elucidate samples distance in the high-dimensional space. In RNA-seq data, each gene is a dimension,
          so the data has tens of thousands of dimensions. SSDA uses Euclidean distance to elucidate samples distance in the
          high-dimensional space, which helps to understand the relationship of samples across exprimental conditions or sample replicates.
          The heatmap clusters samples with similar distances, which makes the results easier to interpret.")
      )
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
