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
output$preview_card <- renderUI({
  if (input_data$values == "geo") {
    box(
      id = "geo_fetch_card", title = "Search GEO Data", closable = F, width = 12, status = NULL, solidHeader = TRUE, collapsible = TRUE,
      tags$table(
        style = "width:100%",
        tags$tr(
          tags$td(textInput("geoID", "GEO Series Accession:", value = "GSE147507", placeholder = "eg. GSE147507", width = "100%")),
          tags$td(style = "padding-bottom: 6px", actionButton("fetch_geo", "Download matrix", width='100%', style = "margin-top:16px", icon = icon("download")))
        )
      ),
      conditionalPanel(
        "!input.fetch_geo",
        column(
          12, style = "border: 1px solid rgb(218,219,220); padding: 5px; margin:0px; border-radius: 8px; background-color: rgb(255, 228, 181)",
          p(style = "text-align: justify;",
            strong("Note:"), "please make sure that the Accession Number you enter belongs to an RNA-seq assay.
          Before starting the analysis, we recommend that you check the data format in the ",
            a(href = "https://www.ncbi.nlm.nih.gov/geo/", "GEO Website", target = "_blank"))
        )
      ),
      conditionalPanel("input.fetch_geo", uiOutput("geo_results"))
    )
  }else {
    box(
      id = "upload_box", title = "Upload local file", width = 12, status = NULL, solidHeader = TRUE, collapsible = T,
      fileInput("file", "Choose input File:", accept = c("text/csv", "text/comma-separated-values,text/plain",
                                                         ".csv"), placeholder = "*(.csv/.txt reads counts file)", width = "100%"),
      checkboxInput(inputId = "header", label = "First row as header ?", value = TRUE, width = "100%"),
      checkboxInput(inputId = "row_names", label = "First column as rownames ?", value = TRUE, width = "100%"),
      fluidRow(
        column(width = 6,actionButton("upload", "Upload >>", class = "run-button",  width='100%')),
        column(width = 6,actionButton("example", "Example >>", class = "run-button",  width='100%'))
      )
    )
  }
})

output$filter_data_card <- renderUI({
  if (input_data$values == "geo") {
    conditionalPanel(
      "input.go_geo_filter",
      box(
        id = "filter_geo_box", title = "Pre-filtering", width = 12, status = NULL, solidHeader = TRUE, collapsible = TRUE,
        uiOutput("gprofiler_species"),
        uiOutput("species_warnning"),
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
        title = "Data table preview", width = 12, status = NULL, solidHeader = TRUE, collapsible = TRUE,
        uiOutput("geo_expr_matrix")
        # uiOutput("filter_geo_text")
      )
    )
  }else {
    box(
      title = "Data filtering", width = 12, status = NULL, solidHeader = TRUE, collapsible = TRUE,
      uiOutput("local_matrix")
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
  data <- readRDS(system.file("extdata", "example.rds", package = "QRAP"))
})

# collapse upload_local_data_card
observe({
  if (is.null(input$example ) | is.null(input$upload))
    return(NULL)
  if (input$example | input$upload) {
    js$collapse("upload_box")

    if (isTRUE(use.example$use)) {
      data <- example()
    }else {
      data <- upload_data()
    }

    output$loadingBox1 <- renderInfoBox({
      infoBox("Samples:", ncol(data), width = 12, color = "light-blue", icon = icon("vials"), fill = TRUE)
    })
    output$loadingBox2 <- renderInfoBox({
      infoBox("Genes:", nrow(data), width = 12, color = "olive", icon = icon("dna"), fill = TRUE)
    })
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

  output$fiteringBox <- renderInfoBox({
    infoBox("Filter Out:", filtered_genes, width = 12, color = "yellow", icon = icon("dna"), fill = TRUE)
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
    theme_bw()+
    theme(axis.text = element_text(size = 16, family = "Times", color = "black"),
          axis.title = element_text(size = 18, family = "Times", color = "black"),
          legend.text = element_text(size = 18, family = "Times", color = "black"))

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
    withSpinner(plotOutput("filter_density", height = "355px"))
  }else {
    withSpinner(dataTableOutput("rawTable"))
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
# # download geofiles using GEOquery package
geo_results <- eventReactive(input$fetch_geo, {
  withProgress(message = "", min = 0, max = 1, value = 0, {
    incProgress(0.6, detail = "Try downloading files ...")
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
      checkboxInput(inputId = "geo_header", label = "First row as header ?", value = TRUE, width = "100%"),
      uiOutput("geo_rownames"),
      uiOutput("geo_columns"),
      fluidRow(
        column(
          6,
          actionButton("preview_geo", "Loading GEO >>", class = "run-button",  width='100%')
        ),
        column(
          6,
          conditionalPanel(
            "input.preview_geo",
            actionButton("go_geo_filter", "GO NEXT >>", class = "run-button",  width='100%')
          )
        )
      )
    )
  )
})

output$geo_rownames <- renderUI({
  if (input$geofile %>% length <= 1) {
    checkboxInput(inputId = "geo_rownames", label = "First column as rownames ?", value = TRUE, width = "100%")
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
            tags$td(numericInput("genes_column", "Column number of geneID:", value = NULL, width = "100%")),
            tags$td(numericInput("reads_column", "Column number of readCounts:", value = NULL, width = "100%"))
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
  if (input$preview_geo) {
    data <- geo_matrix()

    output$geo_loadingBox1 <- renderInfoBox({
      infoBox("Samples:", ncol(data), width = 12, color = "light-blue", icon = icon("vials"), fill = TRUE)
    })
    output$geo_loadingBox2 <- renderInfoBox({
      infoBox("Genes:", nrow(data), width = 12, color = "olive", icon = icon("dna"), fill = TRUE)
    })
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
options = list(pageLength = 3, autoWidth = F, scrollX=TRUE, scrollY=TRUE)
)

# # collapse dataset_preview_card
observeEvent(input$go_geo_filter, {
  js$collapse("geo_fetch_card")
})

observeEvent(input$filter_local, {
  filtered_genes <- dim(geo_matrix()[rowSums(geo_matrix()) < input$genes_n, ])[1]
  left_genes <- dim(geo_matrix()[rowSums(geo_matrix()) >= input$genes_n, ])[1]

  output$geo_fiteringBox <- renderInfoBox({
    infoBox("Filter Out:", filtered_genes, width = 12, color = "yellow", icon = icon("dna"), fill = TRUE)
  })
})

GEO_data <- eventReactive(input$filter_local, {
  # filter data
  geo_matrix()[rowSums(geo_matrix()) >= input$genes_n, ]
})

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
    theme_bw()+
    theme(axis.text = element_text(size = 16, family = "Times", color = "black"),
          axis.title = element_text(size = 18, family = "Times", color = "black"),
          legend.text = element_text(size = 18, family = "Times", color = "black"))

  return(p)
})

output$GEO_density <- renderPlot({
  GEO_density()
})

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
    addSpinner(plotOutput("GEO_density", height = "355px"), spin = "circle")
  }else {
    addSpinner(dataTableOutput("geo_matrix"), spin = "circle")
  }
})

##===============================================Specify the gene and species===============================================##

# get input value of gene types
keyType <- reactive({ input$keyType })

# get input value of OrgDb
OrgDb <- eventReactive(input$filter_local | input$filter_local, {
  if (is.null(input$gprofiler_species))
    return(NULL)
  gprofiler_species <- species()$id[species()$display_name == input$gprofiler_species]

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
  return(data)
})

###----------------------------Info card of local or GEO-------------------
output$info_boxs <- renderUI({
  if (input_data$values == "geo") {
    column(
      width = 2, style = "padding: 0px; margin:0px",
      infoBoxOutput("geo_loadingBox1", width = 12),
      infoBoxOutput("geo_loadingBox2", width = 12),
      infoBoxOutput("geo_fiteringBox", width = 12)
    )
  }else {
    column(
      2, style = "padding: 0px; margin:0px",
      infoBoxOutput("loadingBox1", width = 12),
      infoBoxOutput("loadingBox2", width = 12),
      infoBoxOutput("fiteringBox", width = 12)
    )
  }
})

###----------------------------Introduce Local or GEO-----------------------
output$intro_start <- renderUI({
  if (input_data$values == "geo") {
    column(
      12, style = "padding:0px;",
      fluidRow(
        style = "background-color: rgb(248,249,250); border: 1px solid rgb(218,219,220); padding: 5px; margin:5px; border-radius: 15px;",
        column(
          6, style = "text-align:justify;  border-right: 2px solid white;",
          h3("What is GEO ?"),
          p("The Gene Expression Omnibus (GEO) is a public repository that archives and
            freely distributes comprehensive sets of microarray, next-generation sequencing,
            and other forms of high-throughput functional genomic data submitted by the scientific
            community. GEO records that provide gene expression matrix are organized as follows:"),
          h4("Series"),
          p("A Series record links together a group of related Samples and provides a focal point and
            description of the whole study. Series records may also contain tables describing extracted
            data, summary conclusions, or analyses. Each Series record is assigned a unique and stable
            GEO accession number (GSExxx). The GEO Series mainly deposits tar archive of original raw data files,
            or processed sequence data files."),
          h4("DataSet"),
          p("A GEO Series record is an original submitter-supplied record that summarizes an experiment.
            These data are reassembled by GEO staff into GEO Dataset records (GDSxxx). A DataSet represents
            a curated collection of biologically and statistically comparable GEO Samples.  Samples within
            a DataSet refer to the same Platform, that is, they share a common set of array elements. Value
            measurements for each Sample within a DataSet are assumed to be calculated in an equivalent manner,
            that is, considerations such as background processing and normalization are consistent across the
            DataSet. Information reflecting experimental factors is provided through DataSet subsets.")
        ),
        column(
          6, style = "text-align:justify;",
          h3("How can I query and analyze GEO data?"),
          p("Once you have identified gene expression profiles of interest, there are
            GEO Series Acession Number (GSExxx) or GEO DataSet Acession Number (GDSxxx)
            that help download expression matrix. In our platform, we support you query
            and analysis the value matrix tables within GEO DataSet (GDS) or the Series
            Matrix File or supplementary files within GEO Series (GSExxx)."),
          h4("Download the matrix:"),
          p("After you input the GEO Acession Number and active the 'Download matrix' button,
            We will download the value matrix tables within GDSxxx or supplementary files within
            GSExxx, these files will store in the working directory of the R project you created."),
          h4("Process the matrix:"),
          p("When the files download accomplished, there will show the download files name in the
            Parameter setting panel, and you should select file(s) that contain interested gene
            expression matrix and active the 'Loading GEO' button to preview the matrix."),
          p("Please Note that if the files are in a tar archive format, such as htseq-count generated
            results, these files will conatin Gene ID and Gene Expression Value of each sample, respectively.
            Therefore, you need to provid the column number of the Gene ID and Gene Expression Value to help
            merge the files to generate an analysis ready gene expression matrix.")
        )
      )
    )
  }else {
    column(
      12, style = "padding:0px;",
      fluidRow(
        style = "background-color: rgb(248,249,250); border: 1px solid rgb(218,219,220); padding: 5px; margin:5px; border-radius: 15px;",
        column(
          4, style = "text-align:center;border-right: 2px solid white;",
          strong("Input Example", style = "font-size: 20px"),
          tags$img(src = "images/input_example.jpg",
                   width = "100%")
        ),
        column(
          8, style = "text-align:justify;",
          h3("Why un-normalized counts ?"),
          p("As input, the DESeq2 package expects count data as obtained, e.g.,
            from RNA-seq or another high-throughput sequencing experiment, in the
            form of a matrix of integer values. The value in the i-th row and the j-th
            column of the matrix tells how many reads can be assigned to gene i in sample j.
            The values in the matrix should be un-normalized counts or estimated counts of
            sequencing reads (for single-end RNA-seq) or fragments (for paired-end RNA-seq).
            You can use software like Salmon or Hisat2 + Htseq-count to generate such count matrices.
            It is important to provide count matrices as input for DESeq2’s statistical model to hold,
            as only the count values allow assessing the measurement precision correctly.
            The DESeq2 model internally corrects for library size, so transformed or normalized values such as
            counts scaled by library size should not be used as input."),
          h3("Why Pre-filtering ?"),
          p("While it is not necessary to pre-filter low count genes before running the DESeq2 functions,
            there are two reasons which make pre-filtering useful: by removing rows in which there are very
            few reads, we reduce the memory size of the dds data object, and we increase the speed of the
            transformation and testing functions within DESeq2. Here we perform a minimal pre-filtering to keep
            only rows that have at least 10 reads total. Note that more strict filtering to increase power is
            automatically applied via independent filtering on the mean of normalized counts within the results function.")
        )
      )
    )
  }
})

