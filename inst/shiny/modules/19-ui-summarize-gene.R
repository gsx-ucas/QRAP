fluidPage(
  style = "margin-left: 10px; margin-right:10px;",
  box(
    title = "Run parameters:", width = 4, collapsible = TRUE, solidHeader = TRUE,
    checkboxGroupInput("intgs_genesets", "Gene Sets to explore:", inline = T, width = "100%",
                       choices = c("DEGs", "DEG Pattern", "WGCNA module"),
                       selected = c("DEGs", "DEG Pattern", "WGCNA module")),
    uiOutput("intgs_degs"),
    uiOutput("intgs_degps"),
    uiOutput("intgs_wgcnaps"),
    numericInput("intg_venn_lsize","Size of intersection labels:", value = 1,  min = 0, max = 1, width = "100%"),
    numericInput("intg_venn_nsize","Size of set names:", value = 1,  min = 0, max = 1, width = "100%"),
    actionButton("get_int_genes", "Get intersected genes", class = "run-button", width = "100%")
  ),
  column(
    8,
    tabsetPanel(
      tabPanel(
        "VennDiagram", style = "margin-top: 5px",
        column(
          8,
          fluidRow(
            column(6),
            column(
              6, align = "right",
              dropdownButton(
                numericInput('intg_venn_width', 'Figure Width:', min = 1, max = 20, value = 10, width = "100%"),
                numericInput('intg_venn_height', 'Figure Height:', min = 1, max = 20, value = 10, width = "100%"),
                downloadButton('intg_venn_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
                circle = FALSE, status = "danger", size = "sm", icon = icon("save"), width = "200px",
                right = TRUE, tooltip = tooltipOptions(title = "Click to download figures !")
              )
            )
          ),
          uiOutput("intg_plotUI")
        ),
        column(
          4,
          wellPanel(
            sliderInput("intg_venn_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
            sliderInput("intg_venn_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 420, step = 20, width = "100%")
          )
        )
      ),
      tabPanel(
        "Intersected Table", style = "margin-top: 5px",
        column(
          12,
          uiOutput("intgs_df_id"),
          withSpinner(dataTableOutput("intgs_dfs")),
          downloadButton('intgs_dfs_Csv','Download .csv', class = "btn", width = "100%")
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
    fluidRow(column(3, align = "right", actionLink("psgene", "<< Previous", style = "font-size: 20px")),
             column(6, align = "center", HTML('<p style = "text-align:center;">Copyright &copy; 2022.Shixue All rights reserved.</p>')),
             column(3, align = "left", actionLink("nsgene", "Next >>", style = "font-size: 20px")))
  )
)
