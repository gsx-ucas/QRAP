fluidPage(
  style = "margin-left: 10px; margin-right:10px;",
  box(
    title = "Run parameters:", width = 4, collapsible = TRUE, solidHeader = TRUE,
    uiOutput("intgs_degs"),
    uiOutput("intgs_degps"),
    uiOutput("intgs_wgcnaps"),
    # selectInput("plot_intgs", "Plot Venn:", c("TRUE", "FALSE"), width = "100%"),
    # conditionalPanel(
    #   "input.plot_intgs=='TRUE'",
    #   numericInput("intg_venn_lsize","Size of intersection labels:", value = 1,  min = 0, max = 1, width = "100%"),
    #   numericInput("intg_venn_nsize","Size of set names:", value = 1,  min = 0, max = 1, width = "100%")
    # ),
    numericInput("intg_venn_lsize","Size of intersection labels:", value = 1,  min = 0, max = 1, width = "100%"),
    numericInput("intg_venn_nsize","Size of set names:", value = 1,  min = 0, max = 1, width = "100%"),
    actionButton("get_int_genes", "Get intersected genes", class = "run-button", width = "100%")
  ),
  column(
    6,
    wellPanel(
      dropdownButton(
        numericInput('intg_venn_width', 'Figure Width:', min = 1, max = 20, value = 10, width = "100%"),
        numericInput('intg_venn_height', 'Figure Height:', min = 1, max = 20, value = 10, width = "100%"),
        downloadButton('intg_venn_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
        circle = FALSE, status = "danger", size = "sm",
        icon = icon("save"), width = "200px",
        tooltip = tooltipOptions(title = "Click to download figures !")
      ),
      uiOutput("intg_plotUI")
    )
  ),
  column(
    2,
    wellPanel(
      sliderInput("intg_venn_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
      sliderInput("intg_venn_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 420, step = 20, width = "100%")
    )
  ),
  column(
    12,
    # uiOutput("intgs_df_id"),
    withSpinner(dataTableOutput("intgs_dfs")),
    downloadButton('intgs_dfs_Csv','Download .csv', class = "btn", width = "100%")
  ),
  column(
    12,
    hr(),
    fluidRow(column(3, align = "right", actionLink("psgene", "<< Previous", style = "font-size: 20px")),
             column(6, align = "center", HTML('<p style = "text-align:center;">Copyright &copy; 2022.Shixue All rights reserved.</p>')),
             column(3, align = "left", actionLink("nsgene", "Next >>", style = "font-size: 20px")))
  )
)
