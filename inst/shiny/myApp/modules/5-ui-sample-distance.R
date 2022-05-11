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
    checkboxInput("dis_number", "Show numeric values.", value = F, width = "100%"),
    conditionalPanel("input.dis_number", numericInput("dis_fontsize_number", "Number Fontsize:", value = 10, width = "100%")),
    # actionButton("dis_modal_but", "Additional Parameters for Visualization ...", width = "100%",
    #              style = "background-color: rgb(255,255,255);text-align:left;margin-bottom:10px", icon = icon("plus-square")),
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
      sliderInput("dis_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 450, step = 20, width = "100%")
    )
  ),
  # bsModal(
  #   "dis_modal", "Additional Parameters", "dis_modal_but", size = "large",
  #   fluidPage(
  #     style = "text-align:justify;color:black;background-color:lavender;border-radius:10px;",
  #     h3("Additional Parameters of 'HeatMap plot':"), hr(),
  #     checkboxInput("dis_colname", "Specifying if column names are be shown.", value = FALSE, width = "100%"),
  #     checkboxInput("dis_cluster_rows", "Specifying if rows should be clustered.", value = TRUE, width = "100%"),
  #     checkboxInput("dis_annotation", "Specifies the annotations shown on top of the heatmap.", value = TRUE, width = "100%"),
  #     numericInput("dis_treeheight_row", "The height of a tree for rows:", value = 20, width = "100%"),
  #     numericInput("dis_treeheight_col", "the height of a tree for columns:", value = 20, width = "100%"),
  #     selectInput("dis_angle", "Column names angle (if showed):", choices = c('0', '45', '90', '270', '315'), selected = '315', width = "100%")
  #   )
  # ),
  column(
    12, hr(),
    fluidRow(column(3, align = "right", actionLink("pdis", "<< Previous", style = "font-size: 20px")),
             column(6, align = "center", HTML('<p style = "text-align:center;">Copyright &copy; 2022.Shixue All rights reserved.</p>')),
             column(3, align = "left", actionLink("ndis", "Next >>", style = "font-size: 20px")))
  )
)
