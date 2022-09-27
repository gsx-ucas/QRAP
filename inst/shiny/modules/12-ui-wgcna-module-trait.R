fluidRow(
  style = "margin-left: 10px; margin-right:10px;",
  box(
    title = "Module-Traits Relationship:", width = 4, collapsible = TRUE, solidHeader = TRUE,
    prettyRadioButtons(inputId = "WGCNA_Heatmap_method", label = "Visualize methods:",icon = icon("check"), status = "info",
                       choices = c("pheatmap (external function)", "labeledHeatmap (WGCNA function)"), animation = "jelly", inline = TRUE),
    conditionalPanel(
      "input.WGCNA_Heatmap_method == 'labeledHeatmap (WGCNA function)'",
      numericInput("cex_text", "cex.text:", value = 1, width = "100%"),
      numericInput("yColorWidth", "yColorWidth:", value = 0.05, width = "100%"),
      selectInput("font_lab", "font.labs:", choices = c("1", "2", "3", "4"), selected = "2", width = "100%")
    ),
    conditionalPanel(
      "input.WGCNA_Heatmap_method == 'pheatmap (external function)'",
      numericInput("WGCNA_heatmap_fontsize", "Fontsize for legends:", value = 15, width = "100%"),
      numericInput("WGCNA_heatmap_fontsize_col", "Fontsize  for colnames:", value = 15, width = "100%"),
      numericInput("WGCNA_heatmap_fontsize_num", "Fontsize  for fill text:", value = 8, width = "100%")
    ),
    textInput("module_colors", "Colors:", width = "100%", value = "blue,white,red"),
    uiOutput("module_showRows"),
    uiOutput("module_showCols"),
    actionButton("plot_mtrs", "Plot Module-Traits Relationship Heatmap", width = "100%", class = "plot-button")
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
            tags$h4("Module-Trait Relationshipes Heatmap:")
          ),
          column(
            6, align = "right", style = "padding-top:5px;",
            dropdownButton(
              numericInput('mtrs_heatmap_width', 'Figure Width:', value = 10, width = "100%"),
              numericInput('mtrs_heatmap_height', 'Figure Height:', value = 10, width = "100%"),
              downloadButton('mtrs_heatmap_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
              circle = FALSE, status = "danger", size = "sm", icon = icon("save"), width = "200px",
              right = TRUE, tooltip = tooltipOptions(title = "Click to download figures !")
            )
          )
        )
      ),
      uiOutput("mtrs_heatmapUI")
    )
  ),
  column(
    2,
    wellPanel(
      sliderInput("wgcna_heatmap_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
      sliderInput("wgcna_heatmap_height", "Figure Height (px):", min = 200, max = 1000, value = 541, step = 20, width = "100%")
    ),
    conditionalPanel(
      "input.WGCNA_Heatmap_method == 'pheatmap (external function)'",
      wellPanel(
        checkboxInput("WGCNA_heatmap_cluster_cols", "clustering columns ?", value = TRUE, width = "100%"),
        checkboxInput("WGCNA_heatmap_cluster_rows", "clustering rows ?", value = TRUE, width = "100%")
      )
    )
  ),
  column(
    12, style = "padding:0px;",
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
    fluidRow(column(3, align = "right", actionLink("pWGCNA_4", "<< Previous", style = "font-size: 20px")),
             column(6, align = "center", HTML('<p style = "text-align:center;">Copyright &copy; 2022.Shixue All rights reserved.</p>')),
             column(3, align = "left", actionLink("nWGCNA_4", "Next >>", style = "font-size: 20px")))
  )
)
