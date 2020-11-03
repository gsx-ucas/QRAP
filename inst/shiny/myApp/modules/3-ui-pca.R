fluidPage(
  style = "margin-left:10px;margin-right:10px;",
  box(
    title = "PCA Parameters", width = 4, status = NULL,
    uiOutput("pca_group"),
    # checkboxInput("pca_plotly", "Use Plotly, Help you confirm the sample position.", value = FALSE, width = "100%"),
    numericInput("pca_size", "Point size:", value = 5, width = "100%"),
    numericInput("pca_fontsize", "Fontsize:", value = 15, width = "100%"),
    uiOutput("pca_colorby"),
    actionButton("pca_modal_but", "Additional Parameters for Visualization ...", width = "100%",
                 style = "background-color: rgb(255,255,255);text-align:left;margin-bottom:10px", icon = icon("plus-square")),
    actionButton("plot_pca", "Plotting PCA", width = "100%", class = "plot-button")
  ),
  column(
    6,
    wellPanel(
      dropdownButton(
        numericInput('pca_width', 'Figure Width (cm):', min = 1, max = 20, value = 7, width = "100%"),
        numericInput('pca_height', 'Figure Height (cm):', min = 1, max = 20, value = 5, width = "100%"),
        downloadButton('pca_Pdf','Download .pdf', class = "btn", width = "100%"),
        circle = FALSE, status = "danger", size = "sm", icon = icon("save"), width = "200px",
        tooltip = tooltipOptions(title = "Click to download figures !")
      ),
      uiOutput("PCA_plotUI")
    )
  ),
  column(
    2,
    wellPanel(
      sliderInput("pca_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
      sliderInput("pca_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 450, step = 20, width = "100%")
    )
  ),
  bsModal(
    "pca_modal", "Additional Parameters", "pca_modal_but", size = "large",
    fluidPage(
      style = "text-align:justify;color:black;background-color:lavender;border-radius:10px;",
      h3("Additional Parameters of 'PCA plot':"), hr(),
      selectInput("pca_text_repel", "show names on figure:", choices = c("TRUE","FALSE"), selected = "FALSE",width = "100%"),
      conditionalPanel("input.pca_text_repel == 'TRUE'", numericInput("pca_text_cex", "names text size:", value = 5, width = "100%")),
      HTML(
        paste0(
          '<div class="form-group shiny-input-container" style="width: 100%;">',
          '<label class="control-label" for="pca_ggText">ggplot codes:</label>',
          '<textarea id="pca_ggText" class="form-control" placeholder="theme(text = element_text(face = &#39;bold&#39;))" style="width: 100%;" rows="12"></textarea>',
          '</div>'
        )
      )
    )
  ),
  column(
    12, hr(),
    fluidRow(column(2), column(3, actionLink("pPCA", "<< Previous", style = "font-size: 20px")),
             column(4, p("You are in PCA analysis page ...", style = "color: grey; font-size: 20px")),
             column(3, actionLink("nPCA", "Next >>", style = "font-size: 20px")))
  )
)
