fluidPage(
  style = "margin-left: 10px; margin-right:10px;",
  box(
    title = "Correlation Plot Parameters:", width = 4, status = NULL,
    radioButtons(
      inputId = "corr_type",
      label = "Pairwise or multiple:",
      choices = c("pairwise (scatter)", "multiple (heatmap)", "multiple (scatter)"),
      inline = TRUE, width = "100%"
    ),
    conditionalPanel(
      "input.corr_type=='pairwise (scatter)'",
      uiOutput('Corr_group1'),
      uiOutput('Corr_group2')
    ),
    conditionalPanel(
      "input.corr_type=='multiple (heatmap)' | input.corr_type=='multiple (scatter)'",
      uiOutput("Corr_groups")
    ),
    selectInput("corr_method", "Correlation coefficient method:", c("pearson", "spearman", "kendall"), width = "100%"),
    selectInput("corr_data", "Use which data:", c("log2(normalized_counts + 1)" = "rel_value",
                                                  "rlog or vst transformed value" = "trans_value"), width = "100%"),
    conditionalPanel(
      "input.corr_type=='pairwise (scatter)'",
      numericRangeInput("corr_limits","xlim & ylim range:", value = c(-5, 50), width = "100%")
    ),
    conditionalPanel(
      "input.corr_type=='multiple (heatmap)'",
      selectInput("corr_color", "color:", choices = c('OrRd', 'YlOrRd', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral'), selected = "RdYlBu", width = "100%"),
      plotOutput("color_pal_corr", height = "20px"),
      numericInput("corr_heatsize", "Font size:", value = 12, width = "100%")
    ),
    conditionalPanel(
      "input.corr_type=='multiple (scatter)'",
      numericInput("pairs_size", "Point size:", value = 0.5, width = "100%"),
      numericInput("pairs_fontsize", "Correlations fontsize:", value = 0.5, width = "100%")
    ),
    actionButton("corr_modal_but", "Additional Parameters for Visualization ...", width = "100%",
                 style = "background-color: rgb(255,255,255);text-align:left;margin-bottom:10px", icon = icon("plus-square")),
    actionButton("plot_corr", "Plotting", class = "plot-button", width = "100%")
  ),
  bsModal(
    "sample_corr_modal",
    "Additional Parameters",
    "corr_modal_but",
    size = "large",
    fluidPage(
      style = "text-align:justify;color:black;background-color:lavender;border-radius:10px;border:1px solid black;",
      conditionalPanel(
        "input.corr_type=='pairwise (scatter)'",
        h6("Additional Parameters of 'pairwise (scatter)':"),
        hr(),
        numericInput("corr_size", "Point size:", value = 1, min = 0, max = 5, width = "100%"),
        numericInput("corr_alpha", "Point alpha:", value = 0.8, min = 0, max = 1, width = "100%"),
        textInput("corr_col", "Point color:", placeholder = "eg. black or #000000", value = "black", width = "100%"),
        HTML(
          paste0(
            '<div class="form-group shiny-input-container" style="width: 100%;">',
            '<label class="control-label" for="corr_ggText">ggplot2 codes:</label>',
            '<textarea id="corr_ggText" class="form-control" placeholder="theme(text = element_text(face = &#39;bold&#39;))" style="width: 100%;" rows="12"></textarea>',
            '</div>'
          )
        )
      ),
      conditionalPanel(
        "input.corr_type=='multiple (heatmap)'",
        h6("Additional Parameters of 'multiple (heatmap)':"),
        hr(),
        checkboxInput("corr_number", "Show numeric values on the heatmap.", value = F, width = "100%"),
        numericInput("corr_fontsize_number", "Number Fontsize:", value = 10, width = "100%"),
        numericInput("corr_treeheight_row", "The height of a tree for rows:", value = 20, width = "100%"),
        numericInput("corr_treeheight_col", "the height of a tree for columns:", value = 20, width = "100%")
        ),
        conditionalPanel(
          "input.corr_type=='multiple (scatter)'",
          h6("Additional Parameters of 'sample distance plot':"),
          hr(),
          checkboxInput("pairs_density", "Shows the density plots as well as histograms.", value = TRUE, width = "100%"),
          checkboxInput("pairs_ellipses", "Draws correlation ellipses.", value = TRUE, width = "100%"),
          checkboxInput("pairs_stars", "For those people who like to show the significance
                        of correlations by using magic astricks, set stars=TRUE.", value = FALSE, width = "100%")
      )
    )
  ),
  column(
    6,
    wellPanel(
      dropdownButton(
        numericInput('corrPlot_width', 'Figure Width:', min = 1, max = 20, value = 10, width = "100%"),
        numericInput('corrPlot_height', 'Figure Height:', min = 1, max = 20, value = 8, width = "100%"),
        downloadButton('corrPlot_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
        circle = FALSE, status = "danger", size = "sm",
        icon = icon("save"), width = "200px",
        tooltip = tooltipOptions(title = "Click to download figures !")
      ),
      uiOutput("corr_plotUI")
    )
  ),
  column(
    2,
    wellPanel(
      sliderInput("cor_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
      sliderInput("cor_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 550, step = 20, width = "100%")
    )
  ),
  column(
    12, hr(),
    fluidRow(column(2), column(3, actionLink("pCorr", "<< Previous", style = "font-size: 20px")),
             column(4, p("You are in sample correlation page ...", style = "color: grey; font-size: 20px")),
             column(3, actionLink("nCorr", "Next >>", style = "font-size: 20px")))
  )
)
