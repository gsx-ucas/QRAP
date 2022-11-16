fluidPage(
  style = "margin-left: 10px; margin-right:10px;",
  column(
    4,
    box(
      title = "Run parameters:", id = "run_ppi_card", width = 12, collapsible = TRUE, solidHeader = TRUE,
      radioButtons("ppi_genes", "PPI Genes Source:", c("DEGs", "DEG Patterns", "WGCNA Modules"), inline = T, width = "100%"),
      uiOutput("ppi_group"),
      # uiOutput("ppi_subGene"),
      # uiOutput("required_score"),
      numericInput("required_score", "Threshold of significance to include an interaction:", value = 400, width = "100%"),
      selectInput("network_type", "Network type:", c("functional", "physical"), width = "100%"),
      selectInput("hide_disconnected_nodes", "Hides all proteins that are not connected:", c("TRUE"='1', "FALSE"='0'), width = "100%"),
      selectInput("show_query_node_labels", "Use submitted names as protein labels:", c("TRUE"='1', "FALSE"='0'), width = "100%"),
      selectInput("block_structure_pics_in_bubbles", "Disables structure pictures inside the bubble:", c("TRUE"='1', "FALSE"='0'), width = "100%"),
      actionButton("Init_STRINGdb", "Start STRINGdb", class = "run-button", width = "100%")
    )
  ),
  column(
    6,
    wellPanel(
      style = "border: 1px solid rgb(224,224,224);background-color: white;",
      withSpinner(htmlOutput("PPI_Image"))
    )
  ),
  column(
    2,
    wellPanel(
      sliderInput("ppi_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
      sliderInput("ppi_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 570, step = 20, width = "100%"),
      withSpinner(uiOutput("PPI_weblink"))
    )
  ),
  column(
    12,
    fluidRow(
      style = "background-color: rgb(248,249,250); border: 1px solid rgb(218,219,220); padding: 5px; margin:5px; border-radius: 15px;",
      column(
        4, style = "text-align:center;border-right: 2px solid white; padding-top:15px",
        tags$img(src = "images/demo/ppi_network.jpg",
                 width = "100%")
      ),
      column(
        8, style = "text-align:justify;",
        h3("What is protein to protein interaction (PPI) ?"),
        p("Protein-protein interaction plays key role in predicting the protein function of target proteins. 
          The majority of genes and proteins realize resulting phenotype functions as a set of interactions.
          Protein-protein interactions (PPIs) handle a wide range of biological processes, including cell-to-cell 
          interactions and metabolic and developmental control. "),
        p("PPI databases have become major resources for molecular and systems biology, alongside genomic and proteomic databases.
          However, PPI data, perhaps more than other data types in systems biology, is scattered across a very large number of studies 
          and is investigated by a great variety of experimental and computational methods. Consequently, PPI databases have undertaken 
          large scale integration efforts including curating thousands of journal papers, creating a controlled vocabulary for describing 
          PPI experiments, and defining common formats for PPI data. At the same time they have introduced quality control measures for 
          curation, methods for scoring interactions, and approaches for associating interactions with context. Through these efforts, 
          PPI networks have become a key resource for tasks such as prediction of gene function, identification of disease genes, and drug discovery."),
        p("STRING (https://www.string-db.org) is a database of known and predicted protein-protein interactions. The interactions include direct (physical) 
          and indirect (functional) associations. The database contains information from numerous sources, including experimental repositories, computational 
          prediction methods and public text collections. Each interaction is associated with a combined confidence score that integrates the various evidences.")
      )
    )
  ),
  column(
    12,
    hr(),
    fluidRow(
      style = "margin-bottom:20px",
      column(3, align = "right", actionLink("pPPI", "<< Previous", style = "font-size: 20px")),
      column(6, align = "center"),
      column(3, align = "left", actionLink("nPPI", "Next >>", style = "font-size: 20px"))
    )
  )
)
