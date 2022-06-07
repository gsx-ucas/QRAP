fluidPage(
  style = "margin-left: 10px; margin-right:10px;",
  box(
    title = "Correlation Plot Parameters:", width = 4, status = NULL, solidHeader = TRUE,
    prettyRadioButtons(inputId = "corr_type", label = "Pairwise or multiple:", animation = "jelly", inline = TRUE,
                       choices = c("heatmap", "scatterplot", "pairs.panels"), icon = icon("check"), status = "info"),
    conditionalPanel(
      "input.corr_type=='scatterplot'",
      uiOutput('Corr_group1'),
      uiOutput('Corr_group2')
    ),
    conditionalPanel(
      "input.corr_type=='heatmap' | input.corr_type=='pairs.panels'",
      uiOutput("Corr_groups")
    ),
    selectInput("corr_method", "Correlation coefficient method:", c("pearson", "spearman"), width = "100%"),
    selectInput("corr_data", "Use which data:", c("log2(normalized_counts + 1)" = "rel_value",
                                                  "rlog or vst transformed value" = "trans_value"), width = "100%"),
    conditionalPanel(
      "input.corr_type=='scatterplot'",
      selectInput("corr_theme", "Figure themes:",
                  choices = c("theme_bw", "theme_classic", "theme_test", "theme_linedraw",
                              "theme_light", "theme_minimal", "theme_grey", "theme_gray", "theme_dark"))
    ),
    conditionalPanel(
      "input.corr_type=='heatmap'",
      selectInput("corr_color", "color:", choices = c('OrRd', 'YlOrRd', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral'), selected = "RdYlBu", width = "100%"),
      plotOutput("color_pal_corr", height = "40px"),
      numericInput("corr_heatsize", "Font size:", value = 12, width = "100%")
    ),
    conditionalPanel(
      "input.corr_type=='pairs.panels'",
      numericInput("pairs_size", "Point size:", value = 0.5, width = "100%"),
      numericInput("pairs_fontsize", "Correlations fontsize:", value = 5, width = "100%")
    ),
    conditionalPanel(
      "input.corr_type=='scatterplot' | input.corr_type=='pairs.panels'",
      actionButton("corr_modal_but", "Additional Parameters for Visualization ...", width = "100%",
                   style = "background-color: rgb(255,255,255);text-align:left;margin-bottom:10px", icon = icon("plus-square"))
    ),
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
        "input.corr_type=='scatterplot'",
        h6("Additional Parameters of 'scatterplot':"),
        hr(),
        numericRangeInput("corr_limits","xlim & ylim range:", value = c(-0.5, 25), width = "100%"),
        numericInput("corr_size", "Point size:", value = 1, min = 0, max = 5, width = "100%"),
        numericInput("corr_alpha", "Point alpha:", value = 0.8, min = 0, max = 1, width = "100%"),
        textInput("corr_col", "Point color:", placeholder = "eg. black or #000000", value = "black", width = "100%"),
        textAreaInput("corr_ggText", "ggplot2 codes:", value = 'scale_color_brewer(palette = "Set1")+
          theme(axis.title = element_text(size = 21, face = "bold", color = "black", family = "Times"),
          axis.text = element_text(size = 18, face = "bold", color = "black", family = "Times"),
          text = element_text(size = 18, face = "bold", color = "black", family = "Times"))', width = "100%", rows = 12)
      ),
      conditionalPanel(
        "input.corr_type=='pairs.panels'",
        h6("Additional Parameters of 'scatterplot':"),
        hr(),
        textAreaInput("pairs_ggText", "ggplot2 codes:", value = 'theme(strip.background = element_blank(), 
            strip.text = element_text(color = "black", size = 13),
            axis.text = element_text(color = "black", size = 8),
            text = element_text(colour = "black", size = 10))', width = "100%", rows = 12)
      )
    )
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
            tags$h4("Sample Correlation Plots:")
          ),
          column(
            6, align = "right", style = "padding-top:5px;",
            dropdownButton(
              numericInput('corrPlot_width', 'Figure Width:', min = 1, max = 20, value = 10, width = "100%"),
              numericInput('corrPlot_height', 'Figure Height:', min = 1, max = 20, value = 8, width = "100%"),
              downloadButton('corrPlot_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
              circle = FALSE, status = "danger", size = "sm", icon = icon("save"), width = "200px", right = TRUE,
              tooltip = tooltipOptions(title = "Click to download figures !")
            )
          )
        )
      ),
      uiOutput("corr_plotUI")
    )
  ),
  column(
    2,
    wellPanel(
      sliderInput("cor_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
      sliderInput("cor_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 510, step = 2, width = "100%")
    ),
    conditionalPanel(
      "input.corr_type=='heatmap'",
      wellPanel(
        checkboxInput("corr_colname", "Showing column names ?", value = F, width = "100%"),
        checkboxInput("corr_number", "Showing numeric values ?", value = F, width = "100%"),
        conditionalPanel("input.corr_number", numericInput("corr_fontsize_number", "Value fontsize:", value = 10, width = "100%"))
      )
    )
  ),
  column(
    12, style = "padding:0px;",
    fluidRow(
      style = "background-color: rgb(248,249,250); border: 1px solid rgb(218,219,220); padding: 5px; margin:5px; border-radius: 15px;",
      column(
        4, style = "text-align:center;border-right: 2px solid white;",
        # strong("PCA Example", style = "font-size: 20px"),
        tags$img(src = "images/corr_heatmap_demo.jpg",
                 width = "100%")
      ),
      column(
        8, style = "text-align:justify;",
        h3("What is the Correlation coefficient ?"),
        p("The correlation coefficient is a statistical measure of the strength of the relationship between the 
          relative movements of two variables. The values range between -1.0 and 1.0. A calculated number greater 
          than 1.0 or less than -1.0 means that there was an error in the correlation measurement. A correlation of
          -1.0 shows a perfect negative correlation, while a correlation of 1.0 shows a perfect positive correlation. 
          A correlation of 0.0 shows no linear relationship between the movement of the two variables."),
        h3("How to interpret the Correlation coefficient analysis results ?"),
        p("Correlation statistics can be used in RNA-seq for assessing data quality. For example, a correlation coefficient could
          be calculated to determine the correlation between sample replicates, which indicate the biological replication quality
          of samples. Another example, a correlation coefficient between conditions, such as treatment group and WT control, could
          indicate the treatment effects.")
      )
    )
  ),
  column(
    12, hr(),
    fluidRow(
      style = "margin-bottom:20px",
      column(3, align = "right", actionLink("pCorr", "<< Previous", style = "font-size: 20px")),
      column(6, align = "center"),
      column(3, align = "left", actionLink("nCorr", "Next >>", style = "font-size: 20px"))
    )
  )
)
