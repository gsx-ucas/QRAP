#-----------------Update The condition panel----------------------------##
observe({
  if (input$nStart | input$pPCA) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "deseq")
  }
})

##-----------------Create the condition table----------------------------##
sampleTable <- reactive({
  if (input$reset_condition == "Re-upload") {
    inFile <- input$ds_file
    if(is.null(inFile))
      return(NULL)
    df <- read.table(inFile$datapath, header = T, sep = ',', stringsAsFactors = F)
    for (i in colnames(df)[!colnames(df) %in% "samples"]) {
      if (is.character(df[,i])) {
        df[,i] <- factor(df[,i], levels = unique(df[,i]))
      }
    }
  }else {
    if(is.null(data()))
      return(NULL)
    conditions <- data() %>% colnames %>% stringr::str_replace(replacement = '', pattern = '-.*')
    df <- data.frame(samples = colnames(data()), condition=factor(conditions, levels = unique(conditions)))
  }
  return(df)
})

output$conditionTab <- DT::renderDataTable({
  sampleTable()
},rownames = T, editable = TRUE,
options = list(pageLength = 10, autoWidth = F, scrollX=TRUE, 
               scrollY= if (input$reset_condition == 'Auto produce') {
                 "228px"
               }else {
                 "308px"
               })
)

output$ConditionTab <- downloadHandler(
  filename = function()  {paste0("DesignTab",".csv")},
  content = function(file) {
    write.csv(sampleTable(), file, row.names = F)
  }
)

##-----------------renderUI for formula----------------------------##
output$batch_col <- renderUI({
  selectInput(
    inputId = "batch_col", label = "Group as batch factor:", selected = NULL, width = "100%",
    choices = subset(sampleTable(), select = -c(samples, condition)) %>% colnames
  )
})

output$batch_col2 <- renderUI({
  if (input$batch_methods == "removeBatchEffect") {
    selectInput(
      inputId = "batch_col2", label = "Group as batch factor:", selected = "NULL", width = "100%",
      choices = c("NULL", colnames(sampleTable())[!colnames(sampleTable()) %in% c("samples", "condition", input$batch_col)])
    )
  }
})

output$formula <- renderUI({
  req(input$batch_methods)
  if (input$batch_methods == 'NULL') {
    textInput("formula", "Design formula:", value = "~ condition", width = "100%")
  }else if (input$batch_methods == "removeBatchEffect") {
    req(input$batch_col2)
    if (input$batch_col2 != "NULL") {
      textInput("formula", "Design formula:", value = paste0('~ ', input$batch_col, " + ", input$batch_col2, " + condition"), width = "100%")
    }else {
      textInput("formula", "Design formula:", value = paste0('~ ', input$batch_col, " + condition"), width = "100%")
    }
  }else {
    textInput("formula", "Design formula:", value = paste0('~ ', input$batch_col, " + condition"), width = "100%")
  }
})

##-----------------Runing DESeq function----------------------------##
dds <- eventReactive(input$runDESeq,{
  withProgress(message = "", min = 0, max = 1, value = 0,{
    if (!file.exists("Cache/DESeq_object.rds") | input$run_cache == FALSE) {
      incProgress(0.5, detail = paste("Running DESeq, this will take a while ..."))
      dds <- try(Run.DESeq2(expr.data = data(), sample.data = sampleTable(), formula = input$formula, test = input$deseq_test,
                            fitType = input$deseq_fitType, sfType = input$deseq_sfType, betaPrior = input$deseq_betaPrior,
                            reduced = input$deseq_reduced, minReplicatesForReplace = input$deseq_minReplicatesForReplace,
                            modelMatrixType = input$deseq_modelMatrixType, useT = input$deseq_useT, minmu = input$deseq_minmu))
    }else {
      incProgress(0.5, detail = paste("Loading Existed Object from Cache..."))
      dds <- readRDS("./Cache/DESeq_object.rds")
    }
  })
  return(dds)
})

##-----------------Extracting transformed values----------------------------##
trans_value <- eventReactive(input$runDESeq,{
  withProgress(message = "", min = 0, max = 1, value = 0,{

    if (input$batch_methods == "removeBatchEffect") {
      if (input$batch_col2 != "NULL") {
        batch_col2 <- input$batch_col2
      }
    }else {
      batch_col2 <- NULL
    }

    if (input$trans_method=="rlog") {
      if (input$run_cache != T | !file.exists("./Cache/Deseq_rlog.rds")) {
        incProgress(0.5, detail = paste("transforming values by rlog, this will take a while ..."))
        trans_data <- try(transform_value(object = dds(), blind = as.logical(input$trans_blind), fitType = input$trans_fitType,
                                          nsub = input$trans_nsub, trans.method = "rlog", batch.method = input$batch_methods, batch = input$batch_col, batch2 = batch_col2))
      }else {
        incProgress(0.5, detail = paste("Loading Existed Object from Cache..."))
        trans_data <- readRDS("Cache/Deseq_rlog.rds")
      }
    }else {
      if (input$run_cache != T | !file.exists("./Cache/Deseq_vst.rds")) {
        incProgress(0.5, detail = paste("transforming values by vst, this will take a while ..."))
        trans_data <- try(transform_value(object = dds(), blind = as.logical(input$trans_blind), fitType = input$trans_fitType,
                                          nsub = input$trans_nsub, trans.method = "vst", batch.method = input$batch_methods, batch = input$batch_col, batch2 = batch_col2))
      }else {
        incProgress(0.5, detail = paste("Loading Existed Object from Cache..."))
        trans_data <- readRDS("./Cache/Deseq_vst.rds")
      }
    }
  })
  return(trans_data)
})

norm_value <- eventReactive(input$runDESeq,{
  withProgress(message = "", min = 0, max = 1, value = 0,{
    if (input$run_cache != T | !file.exists("./Cache/Normalized_Values.rds")) {
      data <- log2(BiocGenerics::counts(dds(), normalized=TRUE) %>% as.data.frame() + 1)

      incProgress(0.5, detail = paste("Removing batch effects of Normalized values ..."))
      if (input$batch_methods != 'NULL') {
        if (input$batch_methods == "removeBatchEffect") {
          if (input$batch_col2 != "NULL") {
            batch_col2 <- input$batch_col2
          }
        }else {
          batch_col2 <- NULL
        }
        data <- remove.Batch(expr.data = data, designTable = dds()@colData,
                             batch = input$batch_col, batch2 = batch_col2, design = "condition", method = input$batch_methods)
      }
      saveRDS(data, "./Cache/Normalized_Values.rds")
    }else {
      data <- readRDS("./Cache/Normalized_Values.rds")
    }
  })
  return(data)
})

observeEvent(input$runDESeq,{
  dds()
  if ('try-error' %in% class(dds())) {
    sendSweetAlert(title = "error", text = dds()[1], type = "error", btn_labels = "Close")
  }else {
    trans_value()
    if ('try-error' %in% class(trans_value())) {
      sendSweetAlert(title = "error", text = trans_value()[1], type = "error", btn_labels = "Close")
    }else {
      sendSweetAlert(title = "success", text = "Data normalization and transformation completed !", type = "success")
    }
    norm_value()
  }
})
