fluidPage(
  style = "margin-left: 10px; margin-right:10px;",
  column(
    4,
    box(
      title = "Running parameters:", id = "run_genie_card", width = 12, collapsible = TRUE, solidHeader = TRUE,
      selectInput("genie3_genes", "What genes to used:", width = "100%",
                  c("Differential Genes", "Pattern Genes", "WGCNA Module Genes")),
      uiOutput("genie3_group"),
      selectInput("g3_method", "treeMethod", choices = c("Random Forests" = "RF", "Extra-Trees" = "ET"), width = "100%"),
      selectInput("g3K", "Number of candidate regulators at nodes (K):", choices = c("sqrt", "all"), width = "100%"),
      numericInput("g3_nTrees", "nTrees:", value = 1000, width = "100%"),
      numericInput("g3_core", "Number of cores for parallel computing:", value = 1, width = "100%"),
      numericInput("g3_reportMax", "Maximum number of links to report:", value = 15000, width = "100%"),
      actionButton("run_genie", "Run GENIE3", class = "run-button", width = "100%")
    ),
    conditionalPanel(
      "input.run_genie",
      box(
        title = "Plot parameters:", width = 12, collapsible = TRUE, solidHeader = TRUE,
        numericInput("g3_top_links", "Top n most connective genes:", value = 1000, width = "100%"),
        numericInput("g3_top_genes", "Show labels of top n genes:", value = 20, width = "100%"),
        numericInput("visNetwork_fontsize", "Fontsize of text:", value = 30, width = "100%"),
        numericInput("visNetwork_nodesize", "Nodesize:", value = 15, width = "100%"),
        numericInput("visNetwork_degree", "Highlight nearest nodes degree:", value = 1, width = "100%"),
        selectInput("visNetwork_smooth", "Smooth the links:", c("TRUE", "FALSE"), width = "100%"),
        actionButton("plot_genie3", "Plot GENIE3 NetWork", class = "plot-button", width = "100%")
      )
    )
  ),
  column(
    8,
    tabsetPanel(
      tabPanel(
        "Network Graph", style = "margin-top: 5px",
        column(
          9,
          fluidRow(
            column(6),
            column(
              6, align = "right",
              dropdownButton(
                numericInput('genie3_width', 'Figure Width:', min = 1, max = 20, value = 10, width = "100%"),
                numericInput('genie3_height', 'Figure Height:', min = 1, max = 20, value = 10, width = "100%"),
                downloadButton('g3_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
                circle = FALSE, status = "danger", size = "sm", icon = icon("save"), width = "200px",
                right = TRUE, tooltip = tooltipOptions(title = "Click to download figures !")
              )
            )
          ),
          uiOutput("genie3_PlotUI")
        ),
        column(
          3,
          wellPanel(
            sliderInput("genie3_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
            sliderInput("genie3_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 496, step = 20, width = "100%")
          )
        )
      ),
      tabPanel(
        "Network Table", style = "margin-top: 5px; margin-left: 15px; margin-right: 15px",
        fluidRow(
          column(
            12,
            withSpinner(dataTableOutput("linkList")),
            conditionalPanel(
              "input.plot_genie3",
              downloadButton('linkList_download','Download .csv', width = "100%")
            )
          )
        )
      )
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
    fluidRow(column(3, align = "right", actionLink("pgenie3", "<< Previous", style = "font-size: 20px")),
             column(6, align = "center", HTML('<p style = "text-align:center;">Copyright &copy; 2022.Shixue All rights reserved.</p>')),
             column(3, align = "left", actionLink("ngenie3", "Next >>", style = "font-size: 20px")))
  )
)

