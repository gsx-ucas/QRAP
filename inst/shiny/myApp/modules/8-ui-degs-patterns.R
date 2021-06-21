fluidPage(
  style = "margin-left: 10px; margin-right:10px;",
  column(
    4,
    box(
      id = "degsp_run", title = "Run parameters:", width = 12, collapsible = TRUE,
      switchInput(inputId = "degsp_switch", label = "Use Cache", onStatus = "success", offStatus = "danger", inline = T, labelWidth = "100px"),
      uiOutput('degsp_group'),
      numericInput("degsp_minc","Minimum number of genes in a group that will be return:", value = 30, width = "100%"),
      checkboxInput("degsp_scale", "Scale the expression values by row.", value = TRUE, width = "100%"),
      checkboxInput("degsp_reduce", "Remove genes that are outliers of the cluster distribution.", value = FALSE, width = "100%"),
      # actionButton("degsp_modal_but", "Additional Parameters for Visualization ...", width = "100%",
      #              style = "background-color: rgb(255,255,255);text-align:left;margin-bottom:10px", icon = icon("plus-square")),
      actionButton("run_degsp", "Find genes similarity among samples", class = "run-button", width = "100%")
    ),
    conditionalPanel(
      "input.run_degsp",
      box(
        title = "Plot parameters:", width = 12, collapsible = TRUE,
        radioButtons("degsp_type", "visualize type:", c("BoxPlot", "HeatMap"), width = "100%", inline = T),
        uiOutput("degsp_cluster"),
        uiOutput("degsp_order"),
        conditionalPanel(
          "input.degsp_type == 'BoxPlot'",
          numericInput("degsp_cols","The columns of figs:", value = 4,  min = 0, max = 50, width = "100%"),
          selectInput("degsp_scales", "whether to free x or y axis:", choices = c("fixed", "free", "free_x", "free_y"), width = "100%")
        ),
        conditionalPanel(
          "input.degsp_type == 'HeatMap'",
          textInput("degsp_color", "color:", value = "navy,white,red", width = "100%"),
          numericInput("degsp_fontsize","FontSize:", value = 12, width = "100%")
        ),
        actionButton("plot_degspmodal_but", "Additional Parameters for Visualization ...", width = "100%",
                     style = "background-color: rgb(255,255,255);text-align:left;margin-bottom:10px", icon = icon("plus-square")),
        actionButton("plot_degsp", "Plotting", class = "plot-button", width = "100%")
      )
    )
  ),
  bsModal(
    "plot_degspmodal", "Additional Parameters", "plot_degspmodal_but", size = "large",
    fluidRow(
      style = "padding: 10px; margin: 10px",
      column(
        6, style = "text-align:justify;color:black;background-color:lavender;border-radius:10px;border:1px solid black;", br(),
        h2("Additional Parameters of Box-Line Plot:"), hr(),
        checkboxInput("degsp_points", "Specifying if points are be plotted.", value = TRUE, width = "100%"),
        checkboxInput("degsp_boxes", "Specifying if boxes are be plotted.", value = TRUE, width = "100%"),
        checkboxInput("degsp_lines", "Specifying if lines are be plotted.", value = TRUE, width = "100%"),
        HTML(
          paste0(
            '<div class="form-group shiny-input-container" style="width: 100%;">',
            '<label class="control-label" for="degsp_ggText">ggplot2 codes:</label>',
            '<textarea id="degsp_ggText" class="form-control" placeholder="theme(text = element_text(face = &#39;bold&#39;))" style="width: 100%;" rows="12"></textarea>',
            '</div>'
          )
        )
      ),
      column(
        6, style = "text-align:justify;color:black;background-color:papayawhip;border-radius:10px;border:1px solid black;", br(),
        h2("Additional Parameters of Heatmap:"), hr(),
        checkboxInput("degsp_colname", "Specifying if column names are be shown.", value = TRUE, width = "100%"),
        checkboxInput("degsp_cluster_rows", "Specifying if rows should be clustered.", value = FALSE, width = "100%"),
        checkboxInput("degsp_annoRow", "Specifies the annotations shown on left of the heatmap.", value = TRUE, width = "100%"),
        numericInput("degsp_treeheight_row", "The height of a tree for rows:", value = 20, width = "100%"),
        selectInput("degsp_angle", "Column names angle (if showed):", choices = c('0', '45', '90', '270', '315'), selected = '315', width = "100%")
      )
    )
  ),
  column(
    6,
    wellPanel(
      dropdownButton(
        numericInput('degsp_width', 'Figure Width:', min = 1, max = 20, value = 7, width = "100%"),
        numericInput('degsp_height', 'Figure Height:', min = 1, max = 20, value = 5, width = "100%"),
        downloadButton('degsp_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
        circle = FALSE, status = "danger", size = "sm", icon = icon("save"), width = "200px",
        tooltip = tooltipOptions(title = "Click to download figures !")
      ),
      uiOutput("degsp_plotUI")
    )
  ),
  column(
    2,
    wellPanel(
      sliderInput("degsp_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
      sliderInput("degsp_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 550, step = 20, width = "100%")
    )
  ),
  conditionalPanel(
    "input.plot_degsp", style = "margin-left: 10px;",
    box(
      title = "DEGs pattern table:", width = 12,
      fluidRow(
        column(
          12,
          withSpinner(dataTableOutput("degsp_cluster_tab")),
          downloadButton('degsp_cluster_csv','Download .csv', class = "btn btn-warning", width = "20%")
        )
      )
    )
  ),
  column(
    12,
    hr(),
    fluidRow(column(2), column(3, actionLink("pDegsp", "<< Previous", style = "font-size: 20px")),
             column(4, p("You are in DEG pattern page ...", style = "color: grey; font-size: 20px")),
             column(3, actionLink("nDegsp", "Next >>", style = "font-size: 20px")))
  )
)
