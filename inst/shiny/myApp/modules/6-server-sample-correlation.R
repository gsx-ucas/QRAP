observe({
  if (input$ndis | input$pDEA) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "cor")
  }
})
# # Select first group of samples
output$Corr_group1 <- renderUI({
  selectInput(
    inputId = "corr_group1",
    label = "Select group1:",
    choices = dds()$condition %>% unique %>% as.character,
    width = "100%"
  )
})

output$Corr_group2 <- renderUI({
  selectInput(
    inputId = "corr_group2",
    label = "Select group2:",
    choices = setdiff(dds()$condition %>% unique %>% as.character, input$corr_group1),
    width = "100%"
  )
})

output$Corr_groups <- renderUI({
  # pickerInput("corr_groups", "Select groups:", choices = dds()$condition %>% unique %>% as.character,
  #             selected = dds()$condition %>% unique %>% as.character, width = "100%", multiple = T, 
  #             options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 5))
  selectInput("corr_groups", "Select groups:", choices = dds()$condition %>% unique %>% as.character,
              selected = dds()$condition %>% unique %>% as.character, width = "100%", multiple = T)
})

## ----------------------------- plot colors ----------------------------##
output$color_pal_corr <- renderPlot({
  par(mar=c(0,0,0,0))
  display.brewer.pal(n = 9, name = input$corr_color)
})

## ----------------------------- plot sample correlation ----------------------------##

CorrPlot <- eventReactive(input$plot_corr,{
  if (input$corr_data=="trans_value") {
    data <- assay(trans_value())
  }else if (input$corr_data=="rel_value"){
    data <- counts(dds(), normalized=TRUE)
    data <- log2(data + 1)
  }
  
  if (input$corr_type == "pairwise (scatter)") {
    corr_group1_columns <- dds()$condition %in% input$corr_group1
    corr_group2_columns <- dds()$condition %in% input$corr_group2
    
    CorrAssay <- data.frame(V1=data[ ,corr_group1_columns] %>% rowMeans(), V2=data[ ,corr_group2_columns] %>% rowMeans())
    
    pearsonr <- cor.test(CorrAssay$V1, CorrAssay$V2, method = input$corr_method)
    cor <- pearsonr$estimate %>% round(4)
    
    p <- ggplot(data = NULL)+
      geom_point(aes(CorrAssay$V1, CorrAssay$V2),size=input$corr_size, alpha=input$corr_alpha, col = input$corr_col)+
      geom_label_repel(x=input$corr_limits[2]*0.1, y=input$corr_limits[2]*0.8, aes(label=paste('R =', cor, sep=' ')), cex=5, col='red',data=NULL)+
      xlab(input$corr_group1) + ylab(input$corr_group2)+
      xlim(input$corr_limits[1], input$corr_limits[2]) + ylim(input$corr_limits[1], input$corr_limits[2])+
      theme_classic()
    
    if (nchar(input$corr_ggText != 0)) {
      add_funcs <- strsplit(input$corr_ggText, "\\+")[[1]]
      p <- p + lapply(add_funcs, function(x){
        eval(parse(text = x))
      })
    }
    return(p)
  }else if (input$corr_type == "multiple (heatmap)") {
    sub_data <- data[, dds()$condition %in% input$corr_groups]
    corMatrix <- cor(sub_data, method = input$corr_method)
    if (input$corr_color == 'OrRd') {
      colors <- colorRampPalette(brewer.pal(9, input$corr_color))(100)
    }else {
      colors <- colorRampPalette(rev(brewer.pal(9, input$corr_color)))(100)
    }
    pheatmap(corMatrix, treeheight_col = 20,
             color = colors,
             treeheight_row = 20, show_rownames = T,
             display_numbers = input$corr_number,
             fontsize_number = input$corr_fontsize_number,
             show_colnames = F, fontsize = input$corr_heatsize)
  }else {
    mtx <- lapply(input$corr_groups, function(x) {
      corr_column <- dds()$condition %in% x
      df <- data.frame(V1=data[ ,corr_column] %>% rowMeans)
    }) %>% bind_cols()
    
    rownames(mtx) <- rownames(dds())
    colnames(mtx) <- input$corr_groups
    
    pairs.panels(mtx,
                 rug = F,
                 method = input$corr_method, # correlation method
                 hist.col = "#00AFBB",
                 cex = input$pairs_size,
                 cex.cor = input$pairs_fontsize,
                 density = input$pairs_density,  # show density plots
                 ellipses = input$pairs_ellipses, # show correlation ellipses
                 stars = input$pairs_stars
    )
  }
})

output$corrPlot <- renderPlot({
  CorrPlot()
})

output$corr_plotUI <- renderUI({
  withSpinner(plotOutput("corrPlot", width = paste0(input$cor_plot_width, "%"), height = paste0(input$cor_plot_height, "px")))
})

output$corrPlot_Pdf <- downloadHandler(
  filename = function()  {paste0("sample_correlation_plot",".pdf")},
  content = function(file) {
    if (input$corr_type == "pairwise (scatter)") {
      p <- CorrPlot()
      ggsave(file, p, width = input$corrPlot_width, height = input$corrPlot_height)
    }else if (input$corr_type == "multiple (heatmap)") {
      if (input$corr_data=="trans_value") {
        data <- assay(trans_value())
      }else {
        data <- counts(dds(), normalized=TRUE)
        data <- log2(data + 1)
      }
      sub_data <- data[, dds()$condition %in% input$corr_groups]
      corMatrix <- cor(data, method = "pearson")
      pdf(file, width = input$corrPlot_width, height = input$corrPlot_height)
      pheatmap(corMatrix, show_rownames = T,
               treeheight_col = input$corr_treeheight_col,
               treeheight_row = input$corr_treeheight_row,
               display_numbers = input$corr_number,
               fontsize_number = input$corr_fontsize_number,
               show_colnames = F, fontsize = input$corr_heatsize)
      dev.off()
    }else {
      if (input$corr_data=="trans_value") {
        data <- assay(trans_value())
      }else {
        data <- counts(dds(), normalized=TRUE)
        data <- log2(data + 1)
      }
      
      mtx <- lapply(input$corr_groups, function(x) {
        corr_column <- dds()$condition %in% x
        df <- data.frame(V1=data[ ,corr_column] %>% rowMeans)
      }) %>% bind_cols()
      
      rownames(mtx) <- rownames(dds())
      colnames(mtx) <- input$corr_groups
      
      pdf(file, width = input$corrPlot_width, height = input$corrPlot_height)
      pairs.panels(mtx,
                   rug = F,
                   method = "pearson", # correlation method
                   hist.col = "#00AFBB",
                   cex = input$pairs_size,
                   cex.cor = input$pairs_fontsize,
                   density = TRUE,  # show density plots
                   ellipses = TRUE # show correlation ellipses
      )
      dev.off()
    }
  }
)
