fluidPage(
  style = "margin-left: 10px; margin-right:10px;",
  column(
    4,
    box(
      title = "Running parameters:", id = "run_genie_card", width = 12,collapsible = TRUE,
      selectInput("genie3_genes", "What genes to used:", width = "100%",
                  c("Differential Genes", "Pattern Genes", "WGCNA Module Genes")),
      uiOutput("genie3_ref"),
      uiOutput("genie3_group"),
      actionButton("run_genie", "Run GENIE3", class = "run-button", width = "100%")
    ),
    conditionalPanel(
      "input.run_genie",
      box(
        title = "Plot parameters:", width = 12, collapsible = TRUE,
        radioButtons("genie3_plotTypes", "Plot types:", c("visNetwork", "edgebundle"), inline = T, width = "100%"),
        uiOutput("genie3_Intgenes"),
        sliderInput("top_genie3_genes", "Top n most significant genes:", min = 0, max = 400, value = 100, step = 20, width = "100%"),
        conditionalPanel(
          "input.genie3_plotTypes == 'edgebundle'",
          numericInput("edgebundle_fontsize", "Fontsize of text:", value = 5, width = "100%"),
          numericInput("edgebundle_nodesize", "Nodesize:", value = 5, width = "100%"),
          tags$table(
            style = "width: 100%",
            tags$td(textInput("edgeARCcolor", "Lines color:", value = "lightblue", width = "100%")),
            tags$td(textInput("edgeNDcolor", "Nodes color:", value = "orange", width = "100%"))
          )
        ),
        conditionalPanel(
          "input.genie3_plotTypes == 'visNetwork'",
          numericInput("visNetwork_fontsize", "Fontsize of text:", value = 30, width = "100%"),
          numericInput("visNetwork_nodesize", "Nodesize:", value = 15, width = "100%"),
          numericInput("visNetwork_degree", "Highlight nearest nodes degree:", value = 1, width = "100%"),
          selectInput("visNetwork_smooth", "Smooth the links:", c("TRUE", "FALSE"), width = "100%")
        ),
        actionButton("plot_genie3", "Plot GENIE3 NetWork", class = "plot-button", width = "100%")
      )
    )
  ),
  column(
    6,
    wellPanel(
      conditionalPanel(
        "input.genie3_plotTypes=='edgebundle'",
        dropdownButton(
          numericInput('genie3_width', 'Figure Width:', min = 1, max = 20, value = 10, width = "100%"),
          numericInput('genie3_height', 'Figure Height:', min = 1, max = 20, value = 10, width = "100%"),
          downloadButton('edgebundle_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
          circle = FALSE, status = "danger", size = "sm",
          icon = icon("save"), width = "200px",
          tooltip = tooltipOptions(title = "Click to download figures !")
        )
      ),
      uiOutput("genie3_PlotUI"),
      conditionalPanel(
        "input.genie3_plotTypes=='visNetwork'",
        downloadButton('genie3_visNetwork','Download .html', style = "background-color: black; color:white;", width = "100%")
      )
    )
  ),
  column(
    2,
    wellPanel(
      sliderInput("genie3_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
      sliderInput("genie3_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 620, step = 20, width = "100%")
    )
  ),
  column(
    12,
    withSpinner(dataTableOutput("linkList")),
    conditionalPanel(
      "input.plot_genie3",
      downloadButton('linkList_download','Download .csv', style = "background-color: black; color:white;", width = "100%")
    )
  ),
  column(
    12,
    hr(),
    fluidRow(column(2), column(3, actionLink("pgenie3", "<< Previous", style = "font-size: 20px")),
             column(4, p("You are in GENIE3 notwork page ...", style = "color: grey; font-size: 20px")),
             column(3, actionLink("ngenie3", "Next >>", style = "font-size: 20px")))
  )
)

