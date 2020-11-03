fluidPage(
  style = "margin-left:10px;margin-right:10px;",
  box(
    title = "Hierarchical clustering Parameters", width = 4, status = NULL,
    uiOutput("hiera_group"),
    numericInput("hiera_topn", "Top N variance genes:", value = 500, width = "100%"),
    numericInput("hiera_cutree", "Number of clusters the rows are divided into:", value = 1, width = "100%"),
    numericInput("hiera_cutree_cols", "Number of clusters the columns are divided into:", value = 1, width = "100%"),
    numericInput("hiera_fontsize", "Fontsize:", value = 15, width = "100%"),
    textInput("hiera_color", "color:", value = "navy,white,red", placeholder = "eg. navy,white,red or #000080,#FFFFFF,#FF0000", width = "100%"),
    actionButton("hiera_modal_but", "Additional Parameters for Visualization ...", width = "100%",
                 style = "background-color: rgb(255,255,255);text-align:left;margin-bottom:10px", icon = icon("plus-square")),
    actionButton("plot_hiera", "Plotting HeatMap", width = "100%", class = "plot-button")
  ),
  column(
    6,
    wellPanel(
      dropdownButton(
        numericInput('hiera_width', 'Figure Width (cm):', min = 1, max = 20, value = 7, width = "100%"),
        numericInput('hiera_height', 'Figure Height (cm):', min = 1, max = 20, value = 5, width = "100%"),
        downloadButton('hiera_Pdf','Download .pdf', class = "btn", width = "100%"),
        circle = FALSE, status = "danger", size = "sm", icon = icon("save"), width = "200px",
        tooltip = tooltipOptions(title = "Click to download figures !")
      ),
      uiOutput("hiera_plotUI")
    )
  ),
  column(
    2,
    wellPanel(
      sliderInput("hiera_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
      sliderInput("hiera_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 540, step = 20, width = "100%")
    )
  ),
  bsModal(
    "hiera_modal", "Additional Parameters", "hiera_modal_but", size = "large",
    fluidPage(
      style = "text-align:justify;color:black;background-color:lavender;border-radius:10px;",
      h3("Additional Parameters of 'HeatMap plot':"), hr(),
      checkboxInput("hiera_colname", "Specifying if column names are be shown.", value = FALSE, width = "100%"),
      checkboxInput("hiera_cluster_rows", "Specifying if rows should be clustered.", value = TRUE, width = "100%"),
      checkboxInput("hiera_annotation", "Specifies the annotations shown on top of the heatmap.", value = TRUE, width = "100%"),
      numericInput("hiera_treeheight_row", "The height of a tree for rows:", value = 20, width = "100%"),
      numericInput("hiera_treeheight_col", "the height of a tree for columns:", value = 20, width = "100%"),
      selectInput("hiera_angle", "Column names angle (if showed):", choices = c('0', '45', '90', '270', '315'), selected = '315', width = "100%")
    )
  ),
  column(
    12, hr(),
    fluidRow(column(2), column(3, actionLink("pHiera", "<< Previous", style = "font-size: 20px")),
             column(4, p("You are in hierarchical clustering page ...", style = "color: grey; font-size: 20px")),
             column(3, actionLink("nHiera", "Next >>", style = "font-size: 20px")))
  )
)
