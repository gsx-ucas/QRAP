fluidPage(
  style = "margin-left:10px;margin-right:10px;",
  box(
    title = "disrchical clustering Parameters", width = 4, status = NULL, solidHeader = TRUE,
    uiOutput("dis_group"),
    selectInput("dis_color", "color:", choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                                                   'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds', 'YlGn',
                                                   'YlGnBu', 'YlOrBr', 'YlOrRd'), width = "100%"),
    plotOutput("dis_color_pal", height = "20px"),
    br(),
    numericInput("dis_fontsize", "Fontsize:", value = 14, width = "100%"),
    numericInput("dis_treeheight_row", "The height of a tree for rows:", value = 20, width = "100%"),
    numericInput("dis_treeheight_col", "the height of a tree for columns:", value = 20, width = "100%"),
    actionButton("plot_dis", "Plotting HeatMap", width = "100%", class = "plot-button")
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
            tags$h4("Sample-to-Sample Distance Heatmap:")
          ),
          column(
            6, align = "right", style = "padding-top:5px;",
            dropdownButton(
              numericInput('dis_width', 'Figure Width (cm):', min = 1, max = 20, value = 7, width = "100%"),
              numericInput('dis_height', 'Figure Height (cm):', min = 1, max = 20, value = 5, width = "100%"),
              downloadButton('dis_Pdf','Download .pdf', class = "btn", width = "100%"),
              circle = FALSE, status = "danger", size = "sm", icon = icon("save"), width = "200px", right = TRUE,
              tooltip = tooltipOptions(title = "Click to download figures !")
            )
          )
        )
      ),
      uiOutput("dis_plotUI")
    )
  ),
  column(
    2,
    wellPanel(
      sliderInput("dis_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
      sliderInput("dis_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 448, step = 2, width = "100%")
    ),
    wellPanel(
      checkboxInput("dis_colname", "Showing column names ?", value = FALSE, width = "100%"),
      checkboxInput("dis_number", "Showing numeric values ?", value = FALSE, width = "100%"),
      conditionalPanel("input.dis_number", numericInput("dis_fontsize_number", "Number Fontsize:", value = 10, width = "100%"))
    )
  ),
  column(
    12, style = "padding:0px;",
    fluidRow(
      style = "background-color: rgb(248,249,250); border: 1px solid rgb(218,219,220); padding: 5px; margin:5px; border-radius: 15px;",
      column(
        4, style = "text-align:center;border-right: 2px solid white;",
        # strong("PCA Example", style = "font-size: 20px"),
        tags$img(src = "images/dist_demo.png",
                 width = "100%")
      ),
      column(
        8, style = "text-align:justify;",
        h3("What is sample-to-sample distance analysis (SSDA) ?"),
        p("Sample-to-sample distance analysis (SSDA) is another method to assess sequencing and sample replicates
          quality based on calculated distance between samples. SSDA calculated similarity between samples based on
          distance metrics, which specify how the distance between the input samples. A commonly used approach for
          measuring sample distance in RNA-seq data is to use Euclidean distance."),
        h3("How to interpret the SSDA results ?"),
        p("SSDA can elucidate samples distance in the high-dimensional space. In RNA-seq data, each gene is a dimension,
          so the data has tens of thousands of dimensions. SSDA uses Euclidean distance to elucidate samples distance in the
          high-dimensional space, which helps to understand the relationship of samples across exprimental conditions or sample replicates.
          The heatmap clusters samples with similar distances, which makes the results easier to interpret.")
      )
    )
  ),
  column(
    12, hr(),
    fluidRow(column(3, align = "right", actionLink("pdis", "<< Previous", style = "font-size: 20px")),
             column(6, align = "center", HTML('<p style = "text-align:center;">Copyright &copy; 2022.Shixue All rights reserved.</p>')),
             column(3, align = "left", actionLink("ndis", "Next >>", style = "font-size: 20px")))
  )
)
