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
  virtualSelectInput(
    inputId = "corr_groups",  label = "Select groups:",
    choices = dds()$condition %>% unique %>% as.character,
    selected = dds()$condition %>% unique %>% as.character,
    multiple = TRUE, search = TRUE, width = "100%"
  )
})

## ----------------------------- plot colors ----------------------------##
output$color_pal_corr <- renderPlot({
  par(mar=c(1,0,0,0))
  RColorBrewer::display.brewer.pal(n = 9, name = input$corr_color)
})

## ----------------------------- plot sample correlation ----------------------------##

CorrPlot <- eventReactive(input$plot_corr,{
  if (input$corr_data=="trans_value") {
    data <- assay(trans_value())
  }else if (input$corr_data=="rel_value"){
    data <- counts(dds(), normalized=TRUE)
    data <- log2(data + 1)
  }

  if (input$corr_type == "scatterplot") {
    corr_group1_columns <- dds()$condition %in% input$corr_group1
    corr_group2_columns <- dds()$condition %in% input$corr_group2

    CorrAssay <- data.frame(V1 = data[ ,corr_group1_columns] %>% rowMeans(), V2 = data[ ,corr_group2_columns] %>% rowMeans())

    pearsonr <- stats::cor.test(CorrAssay$V1, CorrAssay$V2, method = input$corr_method)
    cor <- pearsonr$estimate %>% round(4)

    require(ggplot2)
    require(ggrepel)
    p <- ggplot(data = NULL)+
      geom_point(aes(CorrAssay$V1, CorrAssay$V2),size=input$corr_size, alpha=input$corr_alpha, col = input$corr_col)+
      # geom_label_repel(x=input$corr_limits[2]*0.1, y=input$corr_limits[2]*0.8, aes(label=paste('R =', cor, sep=' ')), cex=8, col='red',data=NULL)+
      geom_label_repel(x = min(CorrAssay$V1), y = max(CorrAssay$V2), aes(label=paste('R =', cor, sep=' ')), cex = 8, col = 'red',data = NULL, inherit.aes = F)+
      xlab(input$corr_group1) + ylab(input$corr_group2)+
      xlim(input$corr_limits[1], input$corr_limits[2]) + ylim(input$corr_limits[1], input$corr_limits[2])

    p <- p + eval(parse(text = paste0(input$corr_theme, "()")))

    if (nchar(input$corr_ggText != 0)) {
      add_funcs <- strsplit(input$corr_ggText, "\\+")[[1]]
      p <- p + lapply(add_funcs, function(x){
        eval(parse(text = x))
      })
    }
    return(p)
  }else if (input$corr_type == "heatmap") {
    sub_data <- data[, dds()$condition %in% input$corr_groups]
    corMatrix <- cor(sub_data, method = input$corr_method)
    if (input$corr_color == 'OrRd' | input$corr_color == 'YlOrRd') {
      colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, input$corr_color))(100)
    }else {
      colors <- grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(9, input$corr_color)))(100)
    }
    pheatmap::pheatmap(corMatrix, 
             show_colnames = input$corr_colname,
             color = colors, treeheight_col = 20,
             treeheight_row = 20, show_rownames = T,
             display_numbers = input$corr_number,
             fontsize_number = input$corr_fontsize_number,
             fontsize = input$corr_heatsize)
  }else {
    mtx <- lapply(input$corr_groups, function(x) {
      corr_column <- dds()$condition %in% x
      df <- data.frame(V1=data[ ,corr_column] %>% rowMeans)
    }) %>% dplyr::bind_cols()

    rownames(mtx) <- rownames(dds())
    colnames(mtx) <- input$corr_groups
    
    require(ggplot2)
    ggscatter <- function(data, mapping, ..., pt.size = 5, pt.color = "black") {
      x <- GGally::eval_data_col(data, mapping$x)
      y <- GGally::eval_data_col(data, mapping$y)
      df <- data.frame(x = x, y = y)
      pp <- ggplot(df, aes(x=x, y=y)) +
        ggplot2::geom_point(shape=16, size = pt.size, color = pt.color, alpha = 0.8, show.legend = FALSE) +
        ggplot2::geom_abline(intercept = 0, slope = 1, col="darkred")
      return(pp)
    }
    
    ggdehist <- function(data, mapping, ...) {
      x <- GGally::eval_data_col(data, mapping$x)
      df <- data.frame(x = x)
      pp <- ggplot(df, aes(x=x)) +
        geom_histogram(aes(y=..density..), bins = 10, fill = "#00AFBB", color='black', alpha = 0.8) +
        geom_density(aes(y=..density..))
      return(pp)
    }

    p <- GGally::ggpairs(mtx, progress = F, 
                    diag = list(continuous = GGally::wrap(ggdehist)),
                    lower = list(continuous = GGally::wrap(ggscatter, method="pearson", pt.size = input$pairs_size)),
                    upper = list(continuous = GGally::wrap(GGally::ggally_cor, method = input$corr_method,
                                                           stars = TRUE, align_percent = 0.8, color = "red", size = input$pairs_fontsize)))+
      theme_bw()
    
    if (nchar(input$pairs_ggText != 0)) {
      add_funcs <- strsplit(input$pairs_ggText, "\\+")[[1]]
      p <- p + lapply(add_funcs, function(x){
        eval(parse(text = x))
      })
    }
    return(p)
  }
})

output$corrPlot <- renderPlot({
  CorrPlot()
})

output$corr_plotUI <- renderUI({
  shinycssloaders::withSpinner(plotOutput("corrPlot", width = paste0(input$cor_plot_width, "%"), height = paste0(input$cor_plot_height, "px")))
})

output$corrPlot_Pdf <- downloadHandler(
  filename = function()  {paste0("sample_correlation_plot",".pdf")},
  content = function(file) {
    p <- CorrPlot()
    ggplot2::ggsave(file, p, width = input$corrPlot_width, height = input$corrPlot_height)
  }
)
