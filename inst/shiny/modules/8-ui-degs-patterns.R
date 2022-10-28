fluidPage(
  style = "margin-left: 10px; margin-right:10px;",
  column(
    4,
    box(
      id = "degsp_run", title = "Run parameters:", width = 12, collapsible = TRUE, solidHeader = TRUE,
      switchInput(inputId = "degsp_switch", label = "Use Cache", onStatus = "success", offStatus = "danger", inline = T, labelWidth = "100px"),
      uiOutput('degsp_group'),
      numericInput("degsp_minc","Minimum number of genes in a group that will be return:", value = 30, width = "100%"),
      uiOutput("degp_time"),
      uiOutput("degp_col"),
      checkboxInput("degsp_scale", "Scale the expression values by row.", value = TRUE, width = "100%"),
      checkboxInput("degsp_reduce", "Remove genes that are outliers of the cluster distribution.", value = FALSE, width = "100%"),
      actionButton("run_degsp", "Find genes similarity among samples", class = "run-button", width = "100%")
    ),
    conditionalPanel(
      "input.run_degsp",
      box(
        title = "Plot parameters:", width = 12, collapsible = TRUE, solidHeader = TRUE,
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
  column(
    8,
    tabsetPanel(
      tabPanel(
        "Plots of DEG Patterns",
        fluidPage(
          style = "padding: 10px;margin-left: 0px; margin-right: 0px;",
          column(
            9,
            fluidRow(
              column(
                12, 
                # style = "padding-left:0px;margin-left:0px;padding-right:0px;margin-right:0px;border-bottom:solid 1px rgb(224,224,224)",
                column(6),
                column(
                  6, align = "right", style = "padding-top:5px;",
                  dropdownButton(
                    numericInput('degsp_width', 'Figure Width:', min = 1, max = 20, value = 7, width = "100%"),
                    numericInput('degsp_height', 'Figure Height:', min = 1, max = 20, value = 5, width = "100%"),
                    downloadButton('degsp_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
                    circle = FALSE, status = "danger", size = "sm", icon = icon("save"), width = "200px", right = TRUE,
                    tooltip = tooltipOptions(title = "Click to download figures !")
                  )
                )
              )
            ),
            uiOutput("degsp_plotUI")
          ),
          column(
            3,
            wellPanel(
              sliderInput("degsp_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
              sliderInput("degsp_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 470, step = 10, width = "100%")
            ),
            conditionalPanel(
              "input.degsp_type == 'BoxPlot'",
              wellPanel(
                checkboxInput("degsp_points", "plotting points?", value = FALSE, width = "100%"),
                checkboxInput("degsp_boxes", "plotting boxes?", value = FALSE, width = "100%"),
                checkboxInput("degsp_lines", "plotting lines?", value = TRUE, width = "100%")
              )
            ),
            conditionalPanel(
              "input.degsp_type == 'HeatMap'",
              wellPanel(
                checkboxInput("degsp_cluster_rows", "clustere rows?", value = FALSE, width = "100%"),
                checkboxInput("degsp_colname", "show colnames?", value = TRUE, width = "100%"),
                checkboxInput("degsp_annoRow", "annotate clusters?", value = TRUE, width = "100%")
              )
            )
          )
        )
      ),
      tabPanel(
        "Detail Tabel",
        fluidPage(
          style = "padding: 10px;margin-left: 0px; margin-right: 0px;",
          column(
            12,
            withSpinner(dataTableOutput("degsp_cluster_tab")),
            downloadButton('degsp_cluster_csv','Download .csv', class = "btn btn-warning", width = "20%")
          )
        )
      )
    )
  ),
  bsModal(
    "plot_degspmodal", "Additional Parameters", "plot_degspmodal_but", size = "large",
    fluidRow(
      style = "padding: 10px; margin: 10px",
      column(
        12, style = "text-align:justify;color:black;background-color:lavender;border-radius:10px;border:1px solid black;", br(),
        h2("Additional Parameters of Box-Line Plot:"), hr(),
        # checkboxInput("degsp_points", "Specifying if points are be plotted.", value = TRUE, width = "100%"),
        # checkboxInput("degsp_boxes", "Specifying if boxes are be plotted.", value = TRUE, width = "100%"),
        # checkboxInput("degsp_lines", "Specifying if lines are be plotted.", value = TRUE, width = "100%"),
        HTML(
          paste0(
            '<div class="form-group shiny-input-container" style="width: 100%;">',
            '<label class="control-label" for="degsp_ggText">ggplot2 codes:</label>',
            '<textarea id="degsp_ggText" class="form-control" placeholder="theme(text = element_text(face = &#39;bold&#39;))" style="width: 100%;" rows="12"></textarea>',
            '</div>'
          )
        )
      )
    )
  ),
  column(
    12, style = "padding:0px;",
    fluidRow(
      style = "background-color: rgb(248,249,250); border: 1px solid rgb(218,219,220); padding: 5px; margin:5px; border-radius: 15px;",
      column(
        4, style = "text-align:center;border-right: 2px solid white; padding-top:15px",
        tags$img(src = "images/demo/deg-pattern.png",
                 width = "100%")
      ),
      column(
        8, style = "text-align:justify;",
        h3("What is DEG expression pattern analysis ?"),
        p("Research involved time series experiment would more interested in genes that differentially expressed along 
          time points. We provide the DEGreport pacakge to distinguish different gene expression patterns of differentially 
          expressed genes. For example, we group genes that expression increase over time into one expression pattern,
          and group genes that expression decrease over time into another expression pattern. These differentially expressed genes 
          show different expression patterns over time, suggesting that they may have different biological functions and meanings."),
        h3("How to perform the DEG expression pattern analysis ?"),
        p("To perform DEG expression pattern analysis, you need to specify the goup of DEGs, and we recommend that you select all the goups related
          to the time point that you are interested in, and then specify the minimum number of genes of the detected expression patterns. Then, you need to
          provide the variable that are identified as time point, such as time, experimental conditions, etc. If you do two or more experiments at the same
          time, you can analyze them together, just need to provide another variable to distinguish the experimental groups.")
      )
    )
  ),
  column(
    12,
    hr(),
    fluidRow(
      style = "margin-bottom:20px",
      column(3, align = "right", actionLink("pDegsp", "<< Previous", style = "font-size: 20px")),
      column(6, align = "center"),
      column(3, align = "left", actionLink("nDegsp", "Next >>", style = "font-size: 20px"))
    )
  )
)
