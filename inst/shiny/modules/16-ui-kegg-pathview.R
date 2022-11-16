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
      uiOutput("pathview_iframe")
      # dropdownButton(
      #   numericInput('pathview_width', 'Figure Width:', min = 1, max = 20, value = 10, width = "100%"),
      #   numericInput('pathview_height', 'Figure Height:', min = 1, max = 20, value = 10, width = "100%"),
      #   downloadButton('pathview_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
      #   circle = FALSE, status = "danger", size = "sm",
      #   icon = icon("save"), width = "200px",
      #   tooltip = tooltipOptions(title = "Click to download figures !")
      # ),
      # conditionalPanel(
      #   "input.show_pathview",
      #   uiOutput("pathview_iframe"),
      #   p("*Note: the png format figure is same as the native kegg graph,
      #   while the pdf format figure was re-generated thus will be different, but the regulation network is not change!"),
      #   tags$table(
      #     tags$td(style = "padding: 10px", downloadButton('pathview_Pdf','Download .pdf', class = "btn btn-warning", width = "100%")),
      #     tags$td(style = "padding: 10px", downloadButton('pathview_Png','Download .png', class = "btn btn-warning", width = "100%"))
      #   )
      # )
    )
  ),
  column(
    2,
    wellPanel(
      sliderInput("pathview_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
      sliderInput("pathview_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 582, step = 2, width = "100%"),
      conditionalPanel(
        "input.show_pathview",
        p(style = "text-align:justify", "*Note: the png format figure is same as the native kegg graph,
        while the pdf format figure was re-generated thus will be different, but the regulation network is not change!"),
        fluidRow(
          column(6, style = "padding:1px", downloadButton('pathview_Pdf','.PDF', class = "btn btn-warning", style = "width:100%")),
          column(6, style = "padding:1px", downloadButton('pathview_Png','.PNG', class = "btn btn-warning", style = "width:100%"))
        )
        # tags$table(
        #   tags$td(style = "padding: 2px; width: 100%", downloadButton('pathview_Pdf','.PDF', class = "btn btn-warning", width = "100%")),
        #   tags$td(style = "padding: 2px; width: 100%", downloadButton('pathview_Png','.PNG', class = "btn btn-warning", width = "100%"))
        # )
      )
    )
  ),
  column(
    12,
    fluidRow(
      style = "background-color: rgb(248,249,250); border: 1px solid rgb(218,219,220); padding: 5px; margin:5px; border-radius: 15px;",
      column(
        4, style = "text-align:center;border-right: 2px solid white; padding-top:15px",
        tags$img(src = "images/demo/pathview.png",
                 width = "100%")
      ),
      column(
        8, style = "text-align:justify;",
        h3("What is Pathview ?"),
        p("Pathview is a tool set for pathway based data integration and visualization. 
          It maps and renders a wide variety of biological data on relevant pathway graphs. 
          All users need is to supply their data and specify the target pathway. 
          Pathview automatically downloads the pathway graph data, parses the data file, 
          maps user data to the pathway, and render pathway graph with the mapped data. 
          In addition, Pathview also seamlessly integrates with pathway and gene set (enrichment) 
          analysis tools for large-scale and fully automated analysis."),
        p("Pathview (Luo and Brouwer, 2013) is a stand-alone software package for pathway based data integration 
          and visualization. This package can be divided into four functional modules: the Downloader, Parser, Mapper 
          and Viewer. Mostly importantly, pathview maps and renders user data on relevant pathway graphs."),
        p("Pathview generates both native KEGG view (PNG format) and Graphviz view (PDF format) for pathways. 
          KEGG view keeps all the meta-data on pathways, spacial and temporal information, tissue/cell types, 
          inputs, outputs and connections. This is important for human reading and interpretation of pathway biology. 
          Graphviz view provides better control of node and edge attributes, better view of pathway topology, better 
          understanding of the pathway analysis statistics.")
      )
    )
  ),
  column(
    12,
    hr(),
    fluidRow(
      style = "margin-bottom:20px",
      column(3, align = "right", actionLink("pPathview", "<< Previous", style = "font-size: 20px")),
      column(6, align = "center"),
      column(3, align = "left", actionLink("nPathview", "Next >>", style = "font-size: 20px"))
    )
  )
)
