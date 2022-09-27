fluidPage(
  style = "margin-left: 10px; margin-right:10px;",
  box(
    title = "Plot parameters:", width = 4, collapsible = TRUE, solidHeader = TRUE,
    selectInput("pathview_inherit","inherit KEGG results from:", choices = c("gProfiler2", "clusterProfiler"), width = "100%"),
    # uiOutput("pathview_orgnism"),
    uiOutput("pathview_group"),
    uiOutput("pathview_id"),
    # selectInput("kegg_native", label = "kegg.native:", choices = c("TRUE", "FALSE"), width = "100%"),
    selectInput("key_pos", label = "key.pos:", choices = c("topright", "bottomleft", "bottomright", "topleft"), width = "100%"),
    numericRangeInput("colorbar_limit", "The limit of colorbar values and bins:", value = c(3,10), width = "100%"),
    numericRangeInput("pathview_pdfsize", "The size of pdf to download:", value = c(8,8), width = "100%"),
    numericInput("pathview_cex", "The size of text:", value = 0.5, width = "100%"),
    actionButton("show_pathview", "Show Pathway Graph", class = "plot-button", width = "100%")
  ),
  column(
    6,
    wellPanel(
      uiOutput("pathview_iframe")
      # dropdownButton(
      #   numericInput('pathview_width', 'Figure Width:', min = 1, max = 20, value = 10, width = "100%"),
      #   numericInput('pathview_height', 'Figure Height:', min = 1, max = 20, value = 10, width = "100%"),
      #   downloadButton('pathview_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
      #   circle = FALSE, status = "danger", size = "sm",
      #   icon = icon("save"), width = "200px",
      #   tooltip = tooltipOptions(title = "Click to download figures !")
      # ),
      # conditionalPanel(
      #   "input.show_pathview",
      #   uiOutput("pathview_iframe"),
      #   p("*Note: the png format figure is same as the native kegg graph,
      #   while the pdf format figure was re-generated thus will be different, but the regulation network is not change!"),
      #   tags$table(
      #     tags$td(style = "padding: 10px", downloadButton('pathview_Pdf','Download .pdf', class = "btn btn-warning", width = "100%")),
      #     tags$td(style = "padding: 10px", downloadButton('pathview_Png','Download .png', class = "btn btn-warning", width = "100%"))
      #   )
      # )
    )
  ),
  column(
    2,
    wellPanel(
      sliderInput("pathview_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
      sliderInput("pathview_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 582, step = 2, width = "100%"),
      conditionalPanel(
        "input.show_pathview",
        p(style = "text-align:justify", "*Note: the png format figure is same as the native kegg graph,
        while the pdf format figure was re-generated thus will be different, but the regulation network is not change!"),
        fluidRow(
          column(6, style = "padding:1px", downloadButton('pathview_Pdf','.PDF', class = "btn btn-warning", style = "width:100%")),
          column(6, style = "padding:1px", downloadButton('pathview_Png','.PNG', class = "btn btn-warning", style = "width:100%"))
        )
        # tags$table(
        #   tags$td(style = "padding: 2px; width: 100%", downloadButton('pathview_Pdf','.PDF', class = "btn btn-warning", width = "100%")),
        #   tags$td(style = "padding: 2px; width: 100%", downloadButton('pathview_Png','.PNG', class = "btn btn-warning", width = "100%"))
        # )
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
    fluidRow(column(3, align = "right", actionLink("pPathview", "<< Previous", style = "font-size: 20px")),
             column(6, align = "center", HTML('<p style = "text-align:center;">Copyright &copy; 2022.Shixue All rights reserved.</p>')),
             column(3, align = "left", actionLink("nPathview", "Next >>", style = "font-size: 20px")))
  )
)
