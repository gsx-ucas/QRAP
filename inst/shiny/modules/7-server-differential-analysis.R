observe({
  if (input$nCorr | input$pDegsp) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "dea")
  }
})

## ----------------------------------------------------------------------------##
##  Identify DEGs

# # Select first group of samples
output$dea_ref <- renderUI({
  selectInput(
    inputId = "dea_ref", label = "Select control group:",
    choices = dds()$condition %>% unique %>% as.character, width = "100%"
  )
})

output$dea_group <- renderUI({
  pickerInput(
    inputId = "dea_group", label = "Select treatment groups:",
    setdiff(dds()$condition %>% unique %>% as.character, input$dea_ref),
    selected = setdiff(dds()$condition %>% unique %>% as.character, input$dea_ref)[1],
    multiple = T, width = "100%", options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 5)
  )
})

DesList <- eventReactive(input$get_DEGs, {
  withProgress(message = "", min = 0, max = 1, value = 0,{
    incProgress(0.6, detail = paste("Getting differentially expressed genes ..."))
    GeneList <- try(get.DEGs(dds = dds(), ctrl = input$dea_ref, treat = input$dea_group, p.adjust = input$dea_pval, abs.lfc = input$dea_lfc, save = TRUE))
  })
  return(GeneList)
})

observeEvent(input$get_DEGs,{
  DesList()
  js$collapse("dea_tab")
  if ('try-error' %in% class(DesList())) {
    shinyalert(title = "error", text = DesList()[1], type = "error", confirmButtonText = "Close")
  }else {
    shinyalert(title = "success", text = "Differentially expressed genes which you have selected
               have been generated and saved to 'DEGs' directory !", type = "success")
  }
})

##--------------------------------------------------------
## Visualize DEGs

output$dea_genes <- renderUI({
  if (input$dePlot == "Volcano") {
    mult = FALSE
  }else {
    mult = TRUE
  }
  virtualSelectInput(
    inputId = "dea_genes",  label = "Select DEGs:",
    choices = stringr::str_remove_all(dir("DEGs"), ".csv"),
    selected = stringr::str_remove_all(dir("DEGs"), ".csv")[1],
    multiple = mult, search = TRUE, width = "100%"
  )
  # selectInput(
  #   inputId = "dea_genes", label = "Select DEGs:", choices = stringr::str_remove_all(dir("DEGs"), ".csv"),
  #   selected = stringr::str_remove_all(dir("DEGs"), ".csv")[1], width = "100%", multiple = mult 
  # )
})

observeEvent(input$get_DEGs,{
  if (input$dePlot == "Volcano") {
    mult = FALSE
  }else {
    mult = TRUE
  }
  updateVirtualSelect(
    session = session, inputId = "dea_genes",
    choices = dir("DEGs") %>% stringr::str_remove_all(".csv"),
    selected = stringr::str_remove_all(dir("DEGs"), ".csv")[1]
  )
  # updateSelectInput(
  #   session = session, inputId = "dea_genes",
  #   choices = dir("DEGs") %>% stringr::str_remove_all(".csv"),
  #   selected = stringr::str_remove_all(dir("DEGs"), ".csv")[1]
  # )
})

# # Volcano Plot
VolPlot <- eventReactive(input$plot_volcano,{
  Des_list <- load.DEGs(input$dea_genes)
  # ctrl <- strsplit(names(Des_list)[1], "_vs_")[[1]][2]
  # degroup <- strsplit(names(Des_list)[1], "_vs_")[[1]][1]
  Res_list <- load.REGs(input$dea_genes)
  # Plot_data <- Des_list[[1]]

  p <- ggplot(data = NULL) + lapply(names(Res_list), function(x){
      geom_point(aes(x=Res_list[[x]]$log2FoldChange, y=-log10(Res_list[[x]]$padj)), size = input$vol_size, alpha=input$vol_alpha)
    })+
    # geom_point(aes(x=Plot_data$log2FoldChange, y=-log10(Plot_data$padj)), size = input$vol_size, alpha=input$vol_alpha)+
    geom_vline(xintercept = c(-input$vol_threasholds[2], input$vol_threasholds[2]), lty=3)+
    geom_hline(yintercept = -log10(input$vol_threasholds[1]), lty=3)+
    # coord_cartesian(clip = 'off')+
    coord_cartesian(xlim = c(input$vol_xlimits[1], input$vol_xlimits[2]), ylim = c(-0.5, input$vol_ylimit), clip = 'off')+
    # xlim(input$vol_xlimits[1], input$vol_xlimits[2])+ ylim(-0.5, input$vol_ylimit)+
    labs(x = 'Log2FoldChange', y = '-Log10 adjusted P-value', colour = "DEGs group")+
    theme_classic()

  if (input$dea_genes %>% length > 1) {
    p <- p + lapply(names(Des_list), function(x){
      geom_point(aes(x=Des_list[[x]]$log2FoldChange, y=-log10(Des_list[[x]]$padj), col = x), size = input$vol_size, alpha=input$vol_alpha, show.legend = T)
    })
  }else {
    up <- subset(Res_list[[1]], padj < input$vol_threasholds[1] & log2FoldChange > input$dea_lfc)
    down <- subset(Res_list[[1]], padj < input$vol_threasholds[1] & log2FoldChange < -input$dea_lfc)
    if (input$show_topn > 0) {
      up_topn <- up[order(up$padj, -up$log2FoldChange), ] %>% head(input$show_topn)
      down_topn <- down[order(down$padj, -down$log2FoldChange), ] %>% head(input$show_topn)
      p <- p + geom_point(aes(x=up$log2FoldChange, y = -log10(up$padj)), color='red', size = input$vol_size, alpha=input$vol_alpha)+
        geom_point(aes(x=down$log2FoldChange, y = -log10(down$padj)), color='blue', size = input$vol_size, alpha=input$vol_alpha)+
        geom_text(x=input$vol_xlimits[1]*0.9, y=input$vol_ylimit*0.9, aes(label=paste0('Down: ', dim(down)[1])), col='blue', size = 5, data=NULL)+
        geom_text(x=input$vol_xlimits[2]*0.9, y=input$vol_ylimit*0.9, aes(label=paste0('Up: ', dim(up)[1])), col='red', size = 5, data=NULL)+
        geom_label_repel(data = up_topn, aes(x = log2FoldChange, y = -log10(padj), label = rownames(up_topn)), size = input$vol_text_size, color = "red", max.overlaps = 100)+
        geom_label_repel(data = down_topn, aes(x = log2FoldChange, y = -log10(padj), label = rownames(down_topn)), size = input$vol_text_size, color = "blue", max.overlaps = 100)
    }else {
      p <- p + geom_point(aes(x=up$log2FoldChange, y = -log10(up$padj)), color='red', size = input$vol_size, alpha=input$vol_alpha)+
        geom_point(aes(x=down$log2FoldChange, y = -log10(down$padj)), color='blue', size = input$vol_size, alpha=input$vol_alpha)+
        geom_text(x=input$vol_xlimits[1]*0.9, y=input$vol_ylimit*0.9, aes(label=paste0('Down: ', dim(down)[1])), col='blue', size = 5, data=NULL)+
        geom_text(x=input$vol_xlimits[2]*0.9, y=input$vol_ylimit*0.9, aes(label=paste0('Up: ', dim(up)[1])), col='red', size = 5, data=NULL)
    }
  }

  if (nchar(input$deVol_ggText != 0)) {
    add_funcs <- strsplit(input$deVol_ggText, "\\+")[[1]]
    p <- p + lapply(add_funcs, function(x){
      eval(parse(text = x))
    })
  }
  return(p)
})

output$VolPlot <- renderPlot({
  VolPlot()
})

output$VolPlot_Pdf <- downloadHandler(
  filename = function()  {paste0("DE_Genes_Volcano_Plot",".pdf")},
  content = function(file) {
    p <- VolPlot()
    ggsave(file, p, width = input$VolPlot_width, height = input$VolPlot_height)
  }
)

# # DeGene HeatMap
HeatMap_Data <- eventReactive(input$plot_deheatmap,{
  conditions <- strsplit(input$dea_genes, "_vs_") %>% unlist %>% unique
  sampleTable <- as.data.frame(colData(dds()))[dds()$condition %in% conditions, ]

  Des_list <- load.DEGs(input$dea_genes)
  DeGenes <- lapply(Des_list, function(x){
    rownames(x)
  }) %>% unlist %>% unique

  DeAssay <- assay(trans_value())[DeGenes, sampleTable$samples %>% as.character]

  return(DeAssay)
})

DeGene_heatmap <- eventReactive(input$plot_deheatmap,{
  conditions <- strsplit(input$dea_genes, "_vs_") %>% unlist %>% unique
  sampleTable <- as.data.frame(colData(dds()))[dds()$condition %in% conditions, ]
  annotation_col = data.frame(condition = factor(sampleTable$condition))
  rownames(annotation_col) = sampleTable$samples

  print(HeatMap_Data() %>% head)
  color = colorRampPalette(strsplit(input$deheat_color, ",")[[1]])(100)
  if (isTRUE(input$deheat_colanno)) {
    pheatmap(HeatMap_Data(), col=color, scale = "row",
             annotation_col = annotation_col,
             show_rownames = FALSE, show_colnames = input$deheat_colname,
             cluster_rows = input$deheat_row, cluster_cols = input$deheat_cols,
             treeheight_row = input$deheat_rowh, treeheight_col = input$deheat_colh,
             angle_col = input$deheat_angle, fontsize = input$deheat_fontsize)
  }else {
    pheatmap(HeatMap_Data(), col=color, scale = "row",
             show_rownames = FALSE, show_colnames = input$deheat_colname,
             cluster_rows = input$deheat_row, cluster_cols = input$deheat_cols,
             treeheight_row = input$deheat_rowh, treeheight_col = input$deheat_colh,
             angle_col = input$deheat_angle, fontsize = input$deheat_fontsize)
  }
})

output$DeHeatmap <- renderPlot({
  DeGene_heatmap()
})

output$DeHeatmap_Pdf <- downloadHandler(
  filename = function()  {paste0("DE_Gene_HeatMap_Plot",".pdf")},
  content = function(file) {
    pdf(file, width = input$DeHeatmap_width, height = input$DeHeatmap_height)
    conditions <- strsplit(input$dea_genes, "_vs_") %>% unlist %>% unique
    sampleTable <- as.data.frame(colData(dds()))[dds()$condition %in% conditions, ]
    annotation_col = data.frame(condition = factor(sampleTable$condition))
    rownames(annotation_col) = sampleTable$samples

    color = colorRampPalette(strsplit(input$deheat_color, ",")[[1]])(100)
    if (isTRUE(input$deheat_colanno)) {
      pheatmap(HeatMap_Data(), col=color, scale = "row",
               annotation_col = annotation_col,
               show_rownames = FALSE, show_colnames = input$deheat_colname,
               cluster_rows = input$deheat_row, cluster_cols = input$deheat_cols,
               treeheight_row = input$deheat_rowh, treeheight_col = input$deheat_colh,
               angle_col = input$deheat_angle, fontsize = input$deheat_fontsize)
    }else {
      pheatmap(HeatMap_Data(), col=color, scale = "row",
               show_rownames = FALSE, show_colnames = input$deheat_colname,
               cluster_rows = input$deheat_row, cluster_cols = input$deheat_cols,
               treeheight_row = input$deheat_rowh, treeheight_col = input$deheat_colh,
               angle_col = input$deheat_angle, fontsize = input$deheat_fontsize)
    }
    dev.off()
  }
)

# # Venn Plot
VennGeneList <- eventReactive(input$plot_venn,{
  Des_list <- load.DEGs(input$dea_genes)
  if (input$venn_genes=="Both") {
    GeneList <- lapply(Des_list, function(x){
      rownames(x)
    })
  }else if (input$venn_genes=="Up Regulated Genes") {
    GeneList <- lapply(Des_list, function(x){
      subset(x, log2FoldChange > input$dea_lfc) %>% rownames
    })
  }else {
    GeneList <- lapply(Des_list, function(x){
      subset(x, log2FoldChange < -input$dea_lfc) %>% rownames
    })
  }
  return(GeneList)
})

VennPlot <- eventReactive(input$plot_venn,{
  p <- ggvenn::ggvenn(VennGeneList(), show_percentage = input$venn_percentage %>% as.logical, stroke_size = 0.5, set_name_size = input$venn_nsize, text_size = input$venn_lsize) 
  # venn(VennGeneList(), zcolor = 'style', ilcs = input$venn_lsize, sncs = input$venn_nsize, box = F)
  
  if (nchar(input$deVenn_ggText != 0)) {
    add_funcs <- strsplit(input$deVenn_ggText, "\\+")[[1]]
    p <- p + lapply(add_funcs, function(x){
      eval(parse(text = x))
    })
  }
  return(p)
})

output$VennPlot <- renderPlot({
  VennPlot()
})

output$VennPlot_Pdf <- downloadHandler(
  filename = function()  {paste0("DE_Gene_Venn_Plot",".pdf")},
  content = function(file) {
    p <- VennPlot()
    ggsave(file, p, width = input$VennPlot_width, height = input$VennPlot_height)
    
    # pdf(file, width = input$VennPlot_width, height = input$VennPlot_height)
    # venn(VennGeneList(), zcolor = 'style', ilcs = input$venn_lsize, sncs = input$venn_nsize, box = F)
    # dev.off()
  }
)

# # DeGene BarPlot
DeGene_barPlot <- eventReactive(input$plot_debar,{
  DesList <- load.DEGs(input$dea_genes)
  if (input$debar_split=="yes") {
    Up_GeneList <- lapply(DesList, function(x){
      dim(subset(x, log2FoldChange > input$dea_lfc))[1]
    })

    Down_GeneList <- lapply(DesList, function(x){
      dim(subset(x, log2FoldChange < -input$dea_lfc))[1]
    })

    up_df <- data.frame(dea_group = names(DesList), dea_number = Up_GeneList %>% unlist, Reg_Groups = "Up Reg")
    down_df <- data.frame(dea_group = names(DesList), dea_number = Down_GeneList %>% unlist, Reg_Groups = "Down Reg")
    De_number <- rbind(up_df, down_df)
    De_number$dea_group <- factor(De_number$dea_group, levels = names(DesList))
    De_number$Reg_Groups <- factor(De_number$Reg_Groups, levels = c("Up Reg", "Down Reg"))

    p <- ggplot(data = De_number, aes(x = dea_group, y = dea_number, fill = Reg_Groups))
  }else {
    DEG_list <- lapply(DesList, function(x){
      dim(x)[1]
    })
    De_number <- data.frame(dea_group = names(DesList), dea_number = DEG_list %>% unlist)
    De_number$dea_group <- factor(De_number$dea_group, levels = names(DesList))

    p <- ggplot(data = De_number, aes(x = dea_group, y = dea_number))
  }

  p <- p + geom_bar(stat = "identity", position = position_dodge(width = 1))+
    labs(x = "Sample Groups", y = "Differential Expressed Genes Number", fill = "Groups", vjust = -0.5)+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  if (input$debar_number == "yes") {
    p <- p + geom_text(aes(y = dea_number * 1.01, label = dea_number), position = position_dodge(width = 1))
  }

  if (nchar(input$deBar_ggText != 0)) {
    add_funcs <- strsplit(input$deBar_ggText, "\\+")[[1]]
    p <- p + lapply(add_funcs, function(x){
      eval(parse(text = x))
    })
  }

  return(p)
})

output$De_barPlot <- renderPlot({
  DeGene_barPlot()
})

output$debarPlot_Pdf <- downloadHandler(
  filename = function()  {paste0("DE_Gene_BarPlot",".pdf")},
  content = function(file) {
    p <- DeGene_barPlot()
    ggsave(file, p, width = input$debarPlot_width, height = input$debarPlot_height)
  }
)

## ------------------------------------------
## renderUI of DEGs Plots
output$dea_plotUI <- renderUI({
  if (input$dePlot=='Volcano') {
    wellPanel(
      style = "padding-top:5px; background-color: white",
      fluidRow(
        column(
          12, style = "padding-left:0px;margin-left:0px;padding-right:0px;margin-right:0px;border-bottom:solid 1px rgb(224,224,224)",
          column(
            6, style = "padding-left:10px;",
            tags$h4("Volcano Plot of DEGs:")
          ),
          column(
            6, align = "right", style = "padding-top:5px;",
            dropdownButton(
              numericInput('VolPlot_width', 'Figure Width:', min = 1, max = 20, value = 7, width = "100%"),
              numericInput('VolPlot_height', 'Figure Height:', min = 1, max = 20, value = 5, width = "100%"),
              downloadButton('VolPlot_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
              circle = FALSE, status = "danger", size = "sm", icon = icon("save"), width = "200px", right = TRUE,
              tooltip = tooltipOptions(title = "Click to download figures !")
            )
          )
        )
      ),
      withSpinner(plotOutput("VolPlot", width = paste0(input$dea_plot_width, "%"), height = paste0(input$dea_plot_height, "px")))
    )
  }else if (input$dePlot=='Heatmap') {
    wellPanel(
      style = "padding-top:5px; background-color: white",
      fluidRow(
        column(
          12, style = "padding-left:0px;margin-left:0px;padding-right:0px;margin-right:0px;border-bottom:solid 1px rgb(224,224,224)",
          column(
            6, style = "padding-left:10px;",
            tags$h4("DEG Heatmap:")
          ),
          column(
            6, align = "right", style = "padding-top:5px;",
            dropdownButton(
              numericInput('DeHeatmap_width', 'Figure Width:', min = 1, max = 20, value = 7, width = "100%"),
              numericInput('DeHeatmap_height', 'Figure Height:', min = 1, max = 20, value = 5, width = "100%"),
              downloadButton('DeHeatmap_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
              circle = FALSE, status = "danger", size = "sm", icon = icon("save"), width = "200px", right = TRUE,
              tooltip = tooltipOptions(title = "Click to download figures !")
            )
          )
        )
      ),
      withSpinner(plotOutput("DeHeatmap", width = paste0(input$dea_plot_width, "%"), height = paste0(input$dea_plot_height, "px")))
    )
  }else if (input$dePlot=='Venn') {
    wellPanel(
      style = "padding-top:5px; background-color: white",
      fluidRow(
        column(
          12, style = "padding-left:0px;margin-left:0px;padding-right:0px;margin-right:0px;border-bottom:solid 1px rgb(224,224,224)",
          column(
            6, style = "padding-left:10px;",
            tags$h4("vennDiagram of DEGs:")
          ),
          column(
            6, align = "right", style = "padding-top:5px;",
            dropdownButton(
              numericInput('VennPlot_width', 'Figure Width:', min = 1, max = 20, value = 7, width = "100%"),
              numericInput('VennPlot_height', 'Figure Height:', min = 1, max = 20, value = 5, width = "100%"),
              downloadButton('VennPlot_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
              circle = FALSE, status = "danger", size = "sm", icon = icon("save"), width = "200px", right = TRUE,
              tooltip = tooltipOptions(title = "Click to download figures !")
            )
          )
        )
      ),
      withSpinner(plotOutput("VennPlot", width = paste0(input$dea_plot_width, "%"), height = paste0(input$dea_plot_height, "px")))
    )
  }else if (input$dePlot=='BarPlot') {
    wellPanel(
      style = "padding-top:5px; background-color: white",
      fluidRow(
        column(
          12, style = "padding-left:0px;margin-left:0px;padding-right:0px;margin-right:0px;border-bottom:solid 1px rgb(224,224,224)",
          column(
            6, style = "padding-left:10px;",
            tags$h4("BarPlot of DEG Numbers:")
          ),
          column(
            6, align = "right", style = "padding-top:5px;",
            dropdownButton(
              numericInput('debarPlot_width', 'Figure Width:', min = 1, max = 20, value = 7, width = "100%"),
              numericInput('debarPlot_height', 'Figure Height:', min = 1, max = 20, value = 5, width = "100%"),
              downloadButton('debarPlot_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
              circle = FALSE, status = "danger", size = "sm", icon = icon("save"), width = "200px", right = TRUE,
              tooltip = tooltipOptions(title = "Click to download figures !")
            )
          )
        )
      ),
      withSpinner(plotOutput("De_barPlot", width = paste0(input$dea_plot_width, "%"), height = paste0(input$dea_plot_height, "px")))
    )
  }
})

##--------------------------------------------------------
## Detials of DEGs in table

output$DeResult_Groups <- renderUI({
  selectInput(
    inputId = "DeTab_ID", label = "Groups Of Differential Expressed Genes:",
    choices = dir("DEGs") %>% stringr::str_remove_all(".csv"),
    width = "40%", multiple = F
  )
})

observeEvent(input$get_DEGs,{
  updateSelectInput(
    session = session, inputId = "DeTab_ID",
    choices = dir("DEGs") %>% stringr::str_remove_all(".csv")
  )
})

DeGeneTab <- reactive({
  if(is.null(input$DeTab_ID))
    return(NULL)
  load.DEGs(input$DeTab_ID)[[1]]
})

output$DeGeneTab <- renderDataTable({
  DeGeneTab()
},rownames = T, editable = TRUE,
options = list(pageLength = 5, autoWidth = F, scrollX=TRUE)
)

output$DeGeneTab_CSV <- downloadHandler(
  filename = function()  {
    paste0(input$DeTab_ID, ".csv")
  },
  content = function(file) {
    write.csv(DeGeneTab(), file, row.names = T)
  }
)
