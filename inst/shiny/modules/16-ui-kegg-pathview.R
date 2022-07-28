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
      # dropdownButton(
      #   numericInput('pathview_width', 'Figure Width:', min = 1, max = 20, value = 10, width = "100%"),
      #   numericInput('pathview_height', 'Figure Height:', min = 1, max = 20, value = 10, width = "100%"),
      #   downloadButton('pathview_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
      #   circle = FALSE, status = "danger", size = "sm",
      #   icon = icon("save"), width = "200px",
      #   tooltip = tooltipOptions(title = "Click to download figures !")
      # ),
      conditionalPanel(
        "input.show_pathview",
        uiOutput("pathview_iframe"),
        p("*Note: the png format figure is same as the native kegg graph,
        while the pdf format figure was re-generated thus will be different, but the regulation network is not change!"),
        tags$table(
          tags$td(style = "padding: 10px", downloadButton('pathview_Pdf','Download .pdf', class = "btn btn-warning", width = "100%")),
          tags$td(style = "padding: 10px", downloadButton('pathview_Png','Download .png', class = "btn btn-warning", width = "100%"))
        )
      )
    )
  ),
  column(
    2,
    wellPanel(
      sliderInput("pathview_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
      sliderInput("pathview_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 560, step = 20, width = "100%")
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
