## ------------------ Update Home page
observe({
  if (input$pStart) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "home")
  }
})

##===============================================The begin of get start panel===============================================##
input_data <- reactiveValues(values = "local")

observeEvent(input$Start_Local, {
  input_data$values <- "local"
})

observeEvent(input$Start_Local1, {
  input_data$values <- "local"
})
observeEvent(input$Start_Local2, {
  input_data$values <- "local"
})
observeEvent(input$Start_Local3, {
  input_data$values <- "local"
})
observeEvent(input$Start_Local4, {
  input_data$values <- "local"
})

observeEvent(input$Start_GEO, {
  input_data$values <- "geo"
})

observeEvent(input$Start_GEO1, {
  input_data$values <- "geo"
})
observeEvent(input$Start_GEO2, {
  input_data$values <- "geo"
})
observeEvent(input$Start_GEO3, {
  input_data$values <- "geo"
})
observeEvent(input$Start_GEO4, {
  input_data$values <- "geo"
})

# update get started panel
observe({
  if (input$Start_Local | input$Start_Local1 | input$Start_Local2 | input$Start_Local3 | input$Start_Local4 |
      input$Start_GEO | input$Start_GEO1 | input$Start_GEO2 | input$Start_GEO3 | input$Start_GEO4 | input$pCondition) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "get_start")
  }
})

##===============================================Local RenderUI===============================================##
# # output the ui of Organism
output$Organism <- renderUI({
  if ("Organism" %in% input$fields) {
    textInput("geo_organism", "Organism:", placeholder = "Enter the Organism you want to search", width = "100%")
  }
})

# # output the ui of other search fields
output$Fiels <- renderUI({
  tags$table(
    style = "width:100%",
    if ("DataSet Type" %in% input$fields) {
      tags$tr(
        tags$td(
          selectInput("geo_dataset_lg", "Logical:",
                      choices = c("AND","OR"),
                      width = "100%")
        ),
        tags$td(
          selectInput("geo_dataset", "DataSet Type:",
                      choices = c("Expression profiling by HTS" = "Expression profiling by high throughput sequencing",
                                  "Expression profiling by array" = "Expression profiling by array"),
                      width = "100%")
        )
      )
    },
    if ("Title" %in% input$fields) {
      tags$tr(
        tags$td(
          selectInput("geo_title_lg", "Logical:",
                      choices = c("AND","OR"),
                      width = "100%")
        ),
        tags$td(
          style = "padding-bottom: 1px",
          textInput("geo_title", "Title:", placeholder = "Enter the words contained in the title", width = "100%")
        )
      )
    },
    if ("Description" %in% input$fields) {
      tags$tr(
        tags$td(
          selectInput("geo_description_lg", "Logical:",
                      choices = c("AND","OR"),
                      width = "100%")
        ),
        tags$td(
          style = "padding-bottom: 1px",
          textInput("geo_description", "Description:", placeholder = "Enter the words contained in the description", width = "100%")
        )
      )
    }
  )
})

output$search_geo_card <- renderUI({
  if (input_data$values == "geo") {
    box(
      id = "geo_search_card", title = "Search GEO Data", width = 12, status = NULL, solidHeader = TRUE, collapsible = TRUE,
      checkboxGroupInput("fields", "Search Fields:", inline = T, width = "100%",
                         choices = c("Organism", "DataSet Type", "Title", "Description"),
                         selected = c("Organism", "DataSet Type", "Title", "Description")),
      uiOutput("Organism"),
      uiOutput("Fiels"),
      fluidRow(
        column(6, actionButton("search_GEO", "Search GEO Data Sets", class = "run-button", width = "100%")),
        column(6, actionButton("skip_search_GEO", "Skip Search GEO", class = "plot-button", width = "100%")),
        column(12, align = "center", p("*(skip this step if you know the geo accession ID.)"))
      )
    )
  }
})

output$preview_card <- renderUI({
  if (input_data$values == "geo") {
    conditionalPanel(
      "input.search_GEO | input.skip_search_GEO",
      box(
        id = "geo_fetch_card", title = "Search GEO Data", closable = F, width = 12, status = NULL, solidHeader = TRUE, collapsible = TRUE,
        tags$table(
          style = "width:100%",
          tags$tr(
            tags$td(
              textInput("geoID", "GEO Series Accession:", placeholder = "eg. GSE140466", width = "100%")
            ),
            tags$td(
              style = "padding-bottom: 5px",
              actionButton("fetch_geo", "fetch data", width='100%', style = "margin-top:17px")
            )
          )
        ),
        conditionalPanel("input.fetch_geo", uiOutput("geo_results"))
      )
    )
  }else {
    box(
      id = "upload_box", title = "Upload local file", width = 12, status = NULL, solidHeader = TRUE, collapsible = T,
      fileInput("file", "Choose input File:", accept = c("text/csv", "text/comma-separated-values,text/plain",
                                                         ".csv"), placeholder = "*(.csv/.txt reads counts file)", width = "100%"),
      checkboxInput(inputId = "header", label = "First row as header !", value = TRUE, width = "100%"),
      checkboxInput(inputId = "row_names", label = "First column as rownames !", value = TRUE, width = "100%"),
      fluidRow(
        column(width = 6,actionButton("upload", "Upload >>", class = "run-button",  width='100%')),
        column(width = 6,actionButton("example", "Example >>", class = "run-button",  width='100%'))
      )
    )
  }
})

output$geo_search_res_card <- renderUI({
  conditionalPanel(
    "input.search_GEO",
    box(
      id = "dataset_preview_card", title = "Data Sets preview", width = 12, status = NULL, solidHeader = TRUE, collapsible = TRUE,
      withSpinner(dataTableOutput("geo_datasets"))
    )
  )
})

output$filter_data_card <- renderUI({
  if (input_data$values == "geo") {
    conditionalPanel(
      "input.go_geo_filter",
      box(
        id = "filter_geo_box", title = "Pre-filtering", width = 12, status = NULL, solidHeader = TRUE, collapsible = TRUE,
        uiOutput("gprofiler_species"),
        uiOutput("species_warnning"),
        # bsAlert("species_alert1"),
        selectInput("keyType", "Gene Types:", choices = c("SYMBOL", "ENSEMBL",
                                                          "ENTREZID"), width = "100%"),
        numericInput("genes_n", "Filter out genes that total counts less than:", value = 1, width = "100%"),
        actionButton("filter_local", "Filter Data >>", class = "run-button",  width='100%')
      )
    )
  }else {
    conditionalPanel(
      "input.upload | input.example",
      box(
        inputId = "filter_local_box", title = "Pre-filtering", width = 12, status = NULL, solidHeader = TRUE, collapsible = TRUE,
        uiOutput("gprofiler_species"),
        uiOutput("species_warnning"),
        # bsAlert("species_alert1"),
        selectInput("keyType", "Gene Types:", choices = c("SYMBOL", "ENSEMBL",
                                                          "ENTREZID"), width = "100%"),
        numericInput("genes_n", "Filter out genes that total counts less than:", value = 1, width = "100%"),
        actionButton("filter_local", "Filter Data >>", class = "run-button",  width='100%')
      )
    )
  }
})

output$preview_mat_card <- renderUI({
  if (input_data$values == "geo") {
    conditionalPanel(
      "input.preview_geo",
      box(
        title = "Data table preview", width = 12, status = "info", collapsible = TRUE,
        uiOutput("geo_expr_matrix"),
        uiOutput("filter_geo_text")
        # bsAlert("filter_alert2")
      )
    )
  }else {
    box(
      title = "Data filtering", width = 12, status = "info", collapsible = TRUE,
      # conditionalPanel("input.upload | input.example", addSpinner(dataTableOutput("rawTable"), spin = "circle")),
      # conditionalPanel("filter_local", addSpinner(plotOutput("filter_density"), spin = "circle"), bsAlert("filter_alert1"))
      uiOutput("local_matrix"),
      uiOutput("filter_local_text")
      # bsAlert("filter_alert1")
    )
  }
})

##===============================================Local Part===============================================##

use.example <- reactiveValues(use = FALSE) # define a reactiveValues for example

# logical, if start with upload buttn, value is false
observeEvent(input$upload,{
  use.example$use <- FALSE
})

# logical, if start with example buttn, value is true
observeEvent(input$example,{
  use.example$use <- TRUE
})

# read local data
upload_data <- eventReactive(input$upload,{
  inFile <- input$file
  data <- upload.data(path = inFile$datapath, header = input$header, row_names = input$row_names)
})

# read example data
example <- eventReactive(input$example,{
  data <- readRDS(system.file("extdata", "example.rds", package = "QRseq"))
  # data <- data[order(colnames(data))]
  # data <- round(data, digits = 0)
})

# collapse upload_local_data_card
observe({
  if (is.null(input$example ) | is.null(input$upload))
    return(NULL)
  if (input$example | input$upload) {
    js$collapse("upload_box")
  }
})

observeEvent(input$filter_local, {
  if (isTRUE(use.example$use)) {
    data <- example()
  }else {
    data <- upload_data()
  }
  # creat alert
  filtered_genes <- dim(data[rowSums(data) < input$genes_n, ])[1]
  left_genes <- dim(data[rowSums(data) >= input$genes_n, ])[1]

  output$filter_local_text <- renderUI({
    p(paste0("Filtering out ", filtered_genes, " low expression genes; ", left_genes," were left for subsequent analysis."), style = "font-weight: 800; padding-top: 3px;")
  })

})

# filter out low expression genes
local_data <- eventReactive(input$filter_local, {
  if (isTRUE(use.example$use)) {
    data <- example()
  }else {
    data <- upload_data()
  }
  filtered_data <- data[rowSums(data) >= input$genes_n, ]

  return(filtered_data)
})

# show expression data matrix
output$rawTable <- renderDataTable({
  if (isTRUE(use.example$use)) {
    data <- example()
  }else {
    data <- upload_data()
  }
},
rownames = T,
options = list(pageLength = 5, autoWidth = F, scrollX=TRUE, scrollY=TRUE)
)

# filter out low expression genes
filter_density <- eventReactive(input$filter_local, {
  if (isTRUE(use.example$use)) {
    data <- example()
  }else {
    data <- upload_data()
  }
  # filter data
  filtered_data <- data[rowSums(data) >= input$genes_n, ]

  sum_row <- rowSums(data)
  sum_row0 <- rowSums(filtered_data)

  p <- ggplot(data = NULL) +
    geom_density(aes(x = log2(sum_row + 1), fill = "Before Filter"), alpha = 0.3, show.legend = T)+
    geom_density(aes(x = log2(sum_row0 + 1), fill = "After Filter"), alpha = 0.3, show.legend = T)+
    geom_vline(xintercept = log2(input$genes_n + 1), col = "red", lty = 2)+
    labs(x = "log2(Total counts + 1) Per genes", y = "Density", fill = "")+
    theme_classic()+
    theme(axis.title = element_text(size = 15), axis.text = element_text(size = 12), legend.text = element_text(size = 15))

  return(p)
})

# show filtered expression densityplot
output$filter_density <- renderPlot({
  filter_density()
})

# decied which matrix to show
show_local_mat <- reactiveValues(data = 'example') # define a reactiveValues for show_local_mat

# logical, if click upload buttn, value is org_data
observeEvent(input$upload,{
  show_local_mat$data <- "org_data"
})

# logical, if click example buttn, value is org_data
observeEvent(input$example,{
  show_local_mat$data <- "org_data"
})

# logical, if click filter_local buttn, value is filtered_data
observeEvent(input$filter_local,{
  show_local_mat$data <- "filtered_data"
})

# creat ui output for switch data and filtered data
output$local_matrix <- renderUI({
  if (show_local_mat$data == "filtered_data") {
    addSpinner(plotOutput("filter_density"), spin = "circle")
  }else {
    addSpinner(dataTableOutput("rawTable"), spin = "circle")
  }
})

species <- reactive({
  get_supported_species()
})

# ui output species selections
output$gprofiler_species <- renderUI({
  selectInput("gprofiler_species", "The species:",
              choices = species()$display_name, selected = "Homo sapiens (Human)", width = "100%")
})


observe({
  output$species_warnning <- renderUI({
    if(is.null(input$gprofiler_species))
      return(NULL)
    if (nchar(input$gprofiler_species) == 0) {
      p("Note: Please choose a species !", style = "font-weight: 800; padding-top: 3px; color: orange;")
    }else {
      p(paste0("Note: You have chosen: ", input$gprofiler_species), style = "font-weight: 800; padding-top: 3px; color: navy;")
    }
  })
})

##===============================================GEO Part===============================================##
# # the function to search geo datasets
datasets <- eventReactive(input$search_GEO,{
  withProgress(message = "", value = 0, {
    incProgress(0.5, detail = "searching geo datasets, this will take a while ...")
    results_df <- Search.GEO(organism = input$geo_organism,
                             title = input$geo_title, title_lg = input$geo_title_lg,
                             description = input$geo_description, description_lg = input$geo_description_lg,
                             datasetType = input$geo_dataset, datasetType_lg = input$geo_dataset_lg)
  })
  return(results_df)
})

# # output the datatable of searching results, return an empty info if results is empty
output$geo_datasets <- renderDataTable({
  datasets_tab <<- datasets()
  if (dim(datasets())[2] > 1) {
    datasets_tab$Series_Accession <<- lapply(datasets()$Series_Accession, function(x){
      paste0("<a href='", "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", x, "' target='_blank'>", x,"</a>")
    }) %>% unlist
    datasets_tab$FTP_download <<- lapply(datasets()$FTP_download, function(x){
      paste0("<a href='", gsub(x = x,pattern = "GEO \\(.*\\)",  replacement = ""), "' target='_blank'>", x,"</a>")
    }) %>% unlist
  }
  return(datasets_tab)
}, escape = FALSE,rownames = T,
options = if (dim(datasets())[2] > 1) {
  list(pageLength = 3, autoWidth = T, scrollX=TRUE,
       columnDefs = list(list(width = '400px',
                              targets = c(3, 4)
       )))
}else {
  list(pageLength = 5, autoWidth = F, scrollX=TRUE)
}
)

# collapse this card after click search button
observe({
  if (is.null(input$search_GEO) | is.null(input$skip_search_GEO))
    return(NULL)
  if (input$search_GEO | input$skip_search_GEO) {
    js$collapse("geo_search_card")
  }
})

# # download geofiles using GEOquery package
geo_results <- eventReactive(input$fetch_geo, {
  withProgress(message = "", min = 0, max = 1, value = 0, {
    incProgress(0.6, detail = "Try downloading filess ...")
    GEO.File <- download.GEO(input$geoID, out_dir = paste0("./GEO_Download/"))
  })
  return(GEO.File)
})

# # return the file name as a selectinput options
results_select <- eventReactive(input$fetch_geo, {
  if (is.list(geo_results())==TRUE) {
    selects <- geo_results() %>% names()
  }else {
    selects <- geo_results()
  }
  return(selects)
})

observe({
  geo_results()
  results_select()
  shinyalert(title = "Fetch geo data finished!", type = "success")
})


# # output this ui
output$geo_results <- renderUI({
  fluidRow(
    column(
      12,
      pickerInput("geofile", "Select the results:", choices = results_select(),
                  selected = results_select()[1], width = "100%", multiple = T, options = list(`actions-box` = TRUE)),
      checkboxInput(
        inputId = "geo_header",
        label = "First row as header !",
        value = TRUE,
        width = "100%"
      ),
      uiOutput("geo_rownames"),
      uiOutput("geo_columns"),
      fluidRow(
        column(
          6,
          actionButton("preview_geo", "Preview GEO >>", class = "run-button",  width='100%')
        ),
        column(
          6,
          actionButton("go_geo_filter", "GO NEXT >>", class = "run-button",  width='100%')
        )
      )
    )
  )
})

output$geo_rownames <- renderUI({
  if (input$geofile %>% length <= 1) {
    checkboxInput(
      inputId = "geo_rownames",
      label = "First column as rownames !",
      value = FALSE,
      width = "100%"
    )
  }
})

output$geo_columns <- renderUI({
  if (input$geofile %>% length > 1) {
    fluidRow(
      column(
        12,
        tags$table(
          style = "width: 100%",
          tags$tr(
            tags$td(
              numericInput("genes_column", "Column number of geneID:", value = NULL, width = "100%")
            ),
            tags$td(
              numericInput("reads_column", "Column number of readCounts:", value = NULL, width = "100%")
            )
          )
        )
      )
    )
  }
})
# # collapse dataset_preview_card
observe({
  if(is.null(input$preview_geo))
    return(NULL)
  if (input$preview_geo == 1) {
    js$collapse("dataset_preview_card")
  }
})

geo_matrix <- eventReactive(input$preview_geo, {
  js$collapse("geo_search")
  matrix <- preview.GEO(fetched_res = geo_results(), geoID = input$geoID, geo_files = input$geofile,
                        genes_column = input$genes_column, reads_column = input$reads_column, header = input$geo_header, row_names = input$geo_rownames)
  return(matrix)
})

output$geo_matrix <- renderDataTable({
  geo_matrix()
},rownames = T,
options = list(pageLength = 5, autoWidth = F, scrollX=TRUE, scrollY=TRUE)
)

# # collapse dataset_preview_card
observeEvent(input$go_geo_filter, {
  js$collapse("geo_fetch_card")
})

# # # collapse dataset_preview_card
# observeEvent(input$filter_local, {
#   js$collapse("geo_data_preview_card")
# })

observeEvent(input$filter_local, {
  filtered_genes <- dim(geo_matrix()[rowSums(geo_matrix()) < input$genes_n, ])[1]
  left_genes <- dim(geo_matrix()[rowSums(geo_matrix()) >= input$genes_n, ])[1]
  # createAlert(session, "filter_alert2", "Filter_Alert2", title = NULL,
  #             content = paste0("Filtering out ", filtered_genes, " low expression genes; ", left_genes," were left for subsequent analysis."),
  #             append = FALSE, style = "success")
  output$filter_geo_text <- renderUI({
    p(paste0("Filtering out ", filtered_genes, " low expression genes; ", left_genes," were left for subsequent analysis."), style = "font-weight: 800; padding-top: 3px;")
  })
})

GEO_data <- eventReactive(input$filter_local, {
  # filter data
  geo_matrix()[rowSums(geo_matrix()) >= input$genes_n, ]
})

# output$geo_data <- renderDataTable({
#   GEO_data()
# },rownames = T,
# options = list(pageLength = 5, autoWidth = F, scrollX=TRUE, scrollY=TRUE)
# )

GEO_density <- eventReactive(input$filter_local, {
  data <- geo_matrix()
  filtered_data <- data[rowSums(data) >= input$genes_n, ]

  sum_row <- rowSums(data)
  sum_row0 <- rowSums(filtered_data)

  p <- ggplot(data = NULL) +
    geom_density(aes(x = log2(sum_row + 1), fill = "Before Filter"), alpha = 0.3, show.legend = T)+
    geom_density(aes(x = log2(sum_row0 + 1), fill = "After Filter"), alpha = 0.3, show.legend = T)+
    geom_vline(xintercept = log2(input$genes_n + 1), col = "red", lty = 2)+
    labs(x = "log2(RowSum + 1)", y = "Density", fill = "")+
    theme_classic()

  return(p)
})

output$GEO_density <- renderPlot({
  GEO_density()
})
# # show filtered expression matrix
# output$filter_Table <- renderDataTable({
#   local_data()
# },
# rownames = T,
# options = list(pageLength = 5, autoWidth = F, scrollX=TRUE, scrollY=TRUE)
# )

# decied which matrix to show
show_geo_mat <- reactiveValues(data = 'org_data') # define a reactiveValues for show_local_mat

# logical, if click preview buttn, value is org_data
observeEvent(input$preview_geo,{
  show_geo_mat$data <- "org_data"
})

# logical, if click filter_local buttn, value is filtered_data
observeEvent(input$filter_local,{
  show_geo_mat$data <- "filtered_data"
})

# creat ui output for switch data and filtered data
output$geo_expr_matrix <- renderUI({
  if (show_geo_mat$data == "filtered_data") {
    addSpinner(plotOutput("GEO_density"), spin = "circle")
  }else {
    addSpinner(dataTableOutput("geo_matrix"), spin = "circle")
  }
})

# ui output species selections
# output$gprofiler_species2 <- renderUI({
#   selectInput("gprofiler_species2", "Species from ensembl:", choices = get_supported_species(), width = "100%")
# })
#
# observe({
#   if(is.null(input$gprofiler_species2))
#     return(NULL)
#   if (nchar(input$gprofiler_species2) == 0) {
#     createAlert(session, "species_alert2", "Species_Alert2", title = NULL,
#                 content = "Note: Please choice a species.", append = FALSE, style = "warning")
#   }else {
#     closeAlert(session, "Species_Alert2")
#   }
# })

##===============================================Specify the gene and species===============================================##

# get input value of gene types
keyType <- reactive({
  input$keyType
  # if (input_data$values == "local") {
  #   print(input$keyType)
  #   input$keyType
  # }else {
  #   print(input$geo_keyType)
  #   input$geo_keyType
  # }
})

# get input value of OrgDb
OrgDb <- eventReactive(input$filter_local | input$filter_local, {
  if (is.null(input$gprofiler_species))
    return(NULL)
  gprofiler_species <- species()$id[species()$display_name == input$gprofiler_species]
  # if (input_data$values == "local") {
  #   gprofiler_species <- input$gprofiler_species
  # }else {
  #   gprofiler_species <- input$gprofiler_species2
  # }
  if (gprofiler_species == "hsapiens") {
    db <- "org.Hs.eg.db"
  }else if (gprofiler_species == "mmusculus") {
    db <- "org.Mm.eg.db"
  }else if (gprofiler_species == "rnorvegicus") {
    db <- "org.Rn.eg.db"
  }else if (gprofiler_species == "scerevisiae") {
    db <- "org.Sc.sgd.db"
  }else if (gprofiler_species == "dmelanogaster") {
    db <- "org.Dm.eg.db"
  }else if (gprofiler_species == "athaliana") {
    db <- "org.At.tair.db"
  }else if (gprofiler_species == "drerio") {
    db <- "org.Dr.eg.db"
  }else if (gprofiler_species == "celegans") {
    db <- "org.Ce.eg.db"
  }else if (gprofiler_species == "btaurus") {
    db <- "org.Bt.eg.db"
  }else if (gprofiler_species == "ggallus") {
    db <- "org.Gg.eg.db"
  }else if (gprofiler_species == "clfamiliaris") {
    db <- "org.Cf.eg.db"
  }else if (gprofiler_species == "sscrofa") {
    db <- "org.Ss.eg.db"
  }else if (gprofiler_species == "mmulatta") {
    db <- "org.Mmu.eg.db"
  }else if (gprofiler_species == "xtropicalis") {
    db <- "org.Xl.eg.db"
  }else if (gprofiler_species == "agambiae") {
    db <- "org.Ag.eg.db"
  }else if (gprofiler_species == "ptroglodytes") {
    db <- "org.Pt.eg.db"
  }else if (gprofiler_species == "pfalciparum") {
    db <- "org.Pf.plasmo.db"
  }else {
    db <- NULL
  }
  print(db)
  return(db)
})

# the expression matrix for downstream analyis
data <- reactive({
  if (input_data$values == "local") {
    data <- local_data()
  }else {
    data <- GEO_data()
  }
  data <- round(data, digits = 0)
  # gsg <- goodSamplesGenes(as.data.frame(t(data)), verbose = 3)
  #
  # if (!gsg$allOK) {
  #   # Optionally, print the gene and sample names that were removed:
  #   if (sum(!gsg$goodGenes)>0)
  #     printFlush(paste("Removing", length(rownames(data)[!gsg$goodGenes]), "genes", sep = " "));
  #   # Remove the offending genes and samples from the data:
  #   data = data[gsg$goodGenes, ]
  # }
  return(data)
})
