#' plot the results of DE gene expression pattern
#'
#' @param table input data.frame
#' @param time the times or other conditions
#' @param color column treated as color
#' @param points whether plot points
#' @param boxes whether plot box
#' @param smooth whether smooth
#' @param lines whether plot the lines
#' @param facet where use facet
#' @param facet_col how many columns of figure
#' @param facet_scales facet scales
#' @param angle x axis lable angle
#' @importFrom stats poly
#' @importFrom dplyr inner_join distinct
#' @importFrom ggplot2 ggplot aes_string geom_boxplot geom_point stat_smooth geom_line facet_wrap theme_bw theme element_text element_blank xlab ylab
#'
#' @export
#'
degPlotCluster <- function (table, time, cluster_order = NULL, color = NULL, points = FALSE, boxes = FALSE,
                             smooth = TRUE, lines = TRUE, facet = TRUE, facet_col = 4, facet_scales = "free_x", angle = 45)
{
  stopifnot(class(table) == "data.frame")
  if ("cluster" %in% colnames(table)) {
    counts <- table(distinct(table, genes, cluster)[["cluster"]])
    table <- inner_join(table, data.frame(cluster = as.integer(names(counts)),
                                          tittle = paste0("cluster", names(counts), " - genes: ", counts),
                                          stringsAsFactors = FALSE), by = "cluster")
  }
  table[["line_group"]] = paste(table[["genes"]], table[[color]])
  splan <- length(unique(table[[time]])) - 1L

  if (!is.null(cluster_order)) {
    counts <- counts[cluster_order]
    table$tittle <- factor(table$tittle, levels = paste0("cluster", cluster_order, " - genes: ", counts))
  }else {
    table$tittle <- factor(table$tittle, levels = paste0("cluster", names(counts), " - genes: ", counts))
  }

  if (is.null(color))
    table[[color]] = NULL
  p <- ggplot(table, aes_string(x = time, y = "value", fill = color,
                                color = color))
  if (lines)
    p <- p + geom_line(aes_string(group = "line_group"), color = "grey", alpha = 0.1)
  if (smooth)
    p <- p + stat_smooth(aes_string(x = time, y = "value",
                                    group = color, color = color), se = FALSE, method = "lm",
                         formula = y ~ poly(x, splan))
  if (boxes)
    p <- p + geom_boxplot(alpha = 0, outlier.size = 0, outlier.shape = NA)
  if (points)
    p <- p + geom_point(alpha = 0.4, size = 1, position = position_jitterdodge(dodge.width = 0.9))
  if (facet)
    p <- p + facet_wrap(~tittle, ncol = facet_col, scales = facet_scales)
  p <- p + theme_bw() +
    theme(axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = angle, hjust = 1, vjust = 1),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 12, colour = "black"),
          legend.position = "none") +
    ylab("Z-score of gene abundance") +
    xlab("")
  p
}

#' scatter plot of two samples or two group samples with correlation
#'
#' @param object DESeqDataSet or DESeqTransform, if use DESeqDataSet log2(normalized.counts + 1) values will be used, if use DESeqTransform rlog or vst transformed values will be used
#' @param corr_group1 group1, can be a single sample name or a condition name of a group samples
#' @param corr_group2 group2, can be a single sample name or a condition name of a group samples
#' @param corr_method a character string indicating which correlation coefficient is to be used for the test. One of "pearson", "kendall", or "spearman", can be abbreviated.
#' @param design design formula when runing DESeq
#' @param digits integer indicating the number of decimal places (round) or significant digits (signif) to be used. Negative values are allowed (see ‘Details’).
#' @param pt.size point size in plot
#' @param pt.alpha point alpha
#' @param pt.col point color
#' @importFrom stats cor.test median
#' @importFrom DESeq2 counts
#' @importFrom SummarizedExperiment assay colData
#' @importFrom ggplot2 ggplot geom_point aes_string xlab ylab theme_classic
#' @importFrom ggrepel geom_label_repel
#'
#' @export
#'
plotCorScatter <- function(object, corr_group1, corr_group2, corr_method, design, digits = 4, pt.size, pt.alpha, pt.col) {
  if (inherits(object, "DESeqDataSet")) {
    data <- counts(object, normalized=TRUE)
    data <- log2(data + 1)
  }else if (inherits(object, "DESeqTransform")) {
    data <- assay(object)
  }

  if (corr_group1 %in% colnames(object)) {
    CorrMatrix <- data.frame(group1 = data[ ,corr_group1], group2 = data[ ,corr_group2])
  }else if (corr_group1 %in% colData(object)[, as.character(design)[2]]) {
    group1_ids <- colData(object)[, as.character(design)[2]] %in% corr_group1
    group2_ids <- colData(object)[, as.character(design)[2]] %in% corr_group2
    CorrMatrix <- data.frame(group1 = data[ ,group1_ids] %>% rowMeans(), group2 = data[ ,group2_ids] %>% rowMeans())
  }

  pearsonr <- cor.test(CorrMatrix$group1, CorrMatrix$group2, method = corr_method)
  cor <- pearsonr$estimate %>% round(digits)

  p <- ggplot(data = NULL)+
    geom_point(aes_string(x = group1, y = group2),size = pt.size, alpha = pt.alpha, col = pt.col, data = CorrMatrix)+
    geom_label_repel(x = median(CorrMatrix$group1), y = max(CorrMatrix$group2) - mean(CorrMatrix$group2), aes(label=paste('R =', cor, sep=' ')), cex=5, col='red',data=NULL)+
    xlab(corr_group1) + ylab(corr_group2)+
    theme_classic()
  return(p)
}

#' Dotplot of enriched results
#'
#' @param object enrichResult or compareClusterResult object
#' @param x x axis, GeneRatio or Count
#' @param color color, one of pvalue, qvalue or p.adjust
#' @param showCategory number of terms to show
#' @param terms name of terms to show
#' @param size dot size
#' @param font.size fontsize
#' @importFrom ggplot2 ggplot geom_point aes_string scale_color_continuous guide_colorbar theme_bw xlab ylab theme theme_classic element_text
#'
#' @export
#'
dotplotResults <- function(object, x = "GeneRatio", color = "p.adjust",
                            showCategory = 10, terms = NULL, size = "Count", font.size=12){

  if (inherits(object, "gseaResult")) {
    results_df <- fortify_results(model = object, showCategory = showCategory, by = "NES", terms = terms)
  }else {
    results_df <- fortify_results(model = object, showCategory = showCategory, by = size, terms = terms)
  }
  
  if (is.null(results_df)) {
    return(NULL)
  }else {
    if (inherits(object, "enrichResult") | inherits(object, "gseaResult")) {
      plots <- ggplot(results_df, aes_string(x = x, y = "Description", size = size, color = color))+
        geom_point()+
        scale_color_continuous(low="red", high="blue", name = color, guide=guide_colorbar(reverse=TRUE))+
        ylab(NULL)+
        theme_bw()+
        theme(axis.text = element_text(size = font.size), text = element_text(size = font.size))
    }else {
      plots <- ggplot(results_df, aes_string(x = "Cluster", y = "Description", size = size, color = color))+
        geom_point()+
        scale_color_continuous(low="red", high="blue", name = color, guide=guide_colorbar(reverse=TRUE))+
        theme_bw()+
        ylab(NULL)+
        theme(axis.text = element_text(size = font.size),
              text = element_text(size = font.size),
              axis.text.x = element_text(angle = 45, hjust = 1))
    }
    return(plots)
  }
}

#' Barplot of enriched results
#'
#' @param object enrichResult or compareClusterResult object
#' @param x x axis, GeneRatio or Count
#' @param color color, one of pvalue, qvalue or p.adjust
#' @param showCategory number of terms to show
#' @param terms name of terms to show
#' @param font.size fontsize
#'
#' @export
#'
barplotResults <- function(object, x = "GeneRatio", color = "p.adjust",
                            showCategory = 10, terms = NULL, font.size=12){
  results_df <- fortify_results(model = object, showCategory = showCategory, by = x, terms = terms)

  if (is.null(results_df)) {
    return(NULL)
  }else {
    if (inherits(object, "enrichResult")) {
      plots <- ggplot(data = results_df, aes_string(x = x, y = "Description", fill = color))+
        geom_bar(stat = "identity", position = "dodge", width = 0.9)+
        scale_fill_continuous(low="red", high="blue", name = color, guide=guide_colorbar(reverse=TRUE))+
        theme_bw()+
        ylab(NULL)+
        theme(axis.text = element_text(size = font.size), text = element_text(size = font.size))
    }else {
      results_df$Cluster <- str_remove_all(as.character(results_df$Cluster), pattern = "\n.*")
      plots <- ggplot(data = results_df, aes(x = -log10(p.adjust), y = Description, fill = Cluster))+
        geom_bar(stat = "identity", position = "stack", width = 0.9, orientation = "y")+
        geom_text(aes(label = round(-log10(p.adjust), 1)), position = "stack")+
        theme_bw()+
        ylab(NULL)+
        theme(axis.text = element_text(size = font.size), text = element_text(size = font.size))
    }
    return(plots)
  }
}

#' ggTable of enriched results
#'
#' @param object enrichResult or compareClusterResult object
#' @param by used as x axis and sorted by this value, c('GeneRatio', 'Count')
#' @param color color, one of pvalue, qvalue or p.adjust
#' @param showCategory number of terms to show
#' @param terms name of terms to show
#' @param font.size fontsize
#' @import grid
#' @import gridExtra
#' @importFrom reshape2 dcast
#' @importFrom dplyr bind_cols
#' @importFrom ggplotify as.ggplot
#' @importFrom RColorBrewer brewer.pal
#'
#' @export
#'
ggtableResults <- function(object, by = "GeneRatio", color = "p.adjust",
                            showCategory = 10, terms = NULL, font.size=10){
  # requireNamespace(grid)
  # requireNamespace(gridExtra)
  # requireNamespace(ggplotify)
  # requireNamespace(RColorBrewer)

  results_df <- fortify_results(model = object, showCategory = showCategory, by = by, terms = terms)

  if (is.null(results_df)) {
    return(NULL)
  }else {
    if (inherits(object, "enrichResult")) {
      df <- results_df

      if ("geneID" %in% colnames(df)) {
        df <- subset(df,  select = -geneID)
      }

      if (dim(df)[1] > 1) {
        df <- df[order(df[, color], decreasing = F), ]
      }

      cols <- brewer.pal(3,"YlGn")
      pal <- colorRampPalette(cols)
      P.colors <- pal(nrow(df))

      fontcolours <- matrix("black", nrow(df), ncol(df))
      fontfaces <- matrix("plain", nrow(df), ncol(df))
      colours <- matrix("white", nrow(df), ncol(df))
      colours[, names(df) %in% color] <- P.colors

      if (dim(df)[1] > 1) {
        colours <- colours[order(df[, by], decreasing = T), ]
        df <- df[order(df[, by], decreasing = T), ]
      }

      cols2 <- brewer.pal(3,"OrRd")
      pal2 <- colorRampPalette(cols2)
      G.colors <- pal2(nrow(df)) %>% rev()
      colours[, names(df) %in% by] <- G.colors

      th <- gridExtra::ttheme_default(base_size = font.size, padding = grid::unit(c(4,4), "mm"), margin = grid::unit(c(1, 1), "mm"),
                                      core = list(bg_params = list(fill = colours, col = "black", lwd = 0.5), fg_params = list(hjust = 0, x = 0.02, col = fontcolours, fontface = fontfaces)),
                                      colhead = list(bg_params = list(fill = "#D3D3D3", lwd = 0.5, col = "black"), fg_params = list(col = "gray39", fontface = "bold")),
                                      rowhead = list(fg_params = list(col = "black", fontface = "bold")))

      g <- tableGrob(df, rows = NULL, theme = th)
      p <- as.ggplot(g)
    }else {
      cols <- c(by, color)

      sub_df <- results_df[, names(results_df) %in% c("Cluster", "ID", "Description", cols)]
      sub_df$Cluster <- str_remove(as.character(sub_df$Cluster), pattern = "\n.*")

      bind_df <- lapply(cols, function(x){
        df <- dcast(sub_df,ID+Description~Cluster, value.var = x)
        names(df)[-c(1:2)] <- paste0(names(df)[-c(1:2)], "_", x)
        return(df)
      }) %>% bind_cols()

      col_names <- lapply(cols, function(x){
        df <- dcast(sub_df,ID+Description~Cluster, value.var = x)
        names(df)[-c(1:2)] <- paste0(names(df)[-c(1:2)], "_", x)
      }) %>% unlist() %>% sort()

      df <- cbind(bind_df[, 1:2], bind_df[, col_names])
      rownames(df) <- df$Description

      fontcolours <- matrix("black", nrow(df), ncol(df))
      fontfaces <- matrix("plain", nrow(df), ncol(df))
      colours <- matrix("white", nrow(df), ncol(df))
      rownames(colours) <- df$Description

      cols <- brewer.pal(3,"YlGn")
      pal<-colorRampPalette(cols)

      cols2 <- brewer.pal(3,"OrRd")
      pal2 <- colorRampPalette(cols2)

      for (i in col_names) {
        if (grepl(by, i)) {
          if (dim(df)[1] > 1) {
            idx1 <- order(df[, i], decreasing = T)
            df <- df[idx1, ]
            colours <- colours[idx1, ]
          }

          G.colors <- pal2(nrow(df)) %>% rev()
          G.colors[is.na(df[, i])] <- NA

          colours[, names(df) %in% i] <- G.colors
        }else {
          if (dim(df)[1] > 1) {
            idx2 <- order(df[, i], decreasing = F)
            df <- df[idx2, ]
            colours <- colours[idx2, ]
          }

          P.colors <- pal(nrow(df))
          P.colors[is.na(df[, i])] <- NA

          colours[, names(df) %in% i] <- P.colors
        }
      }

      df <- df[unique(results_df$Description), ]
      colours <- colours[unique(results_df$Description), ]

      th <- gridExtra::ttheme_default(base_size = font.size, padding = grid::unit(c(4, 4), "mm"), margin = grid::unit(c(1, 1), "mm"),
                                      core = list(bg_params = list(fill = colours, col = "black", lwd = 0.5), fg_params = list(hjust = 0, x = 0.02, col = fontcolours, fontface = fontfaces)),
                                      colhead = list(bg_params = list(fill = "#D3D3D3", lwd = 0.5, col = "black"), fg_params = list(col = "gray39", fontface = "bold")),
                                      rowhead = list(fg_params = list(col = "black", fontface = "bold")))

      g <- tableGrob(df, rows = NULL, theme = th)
      p <- as.ggplot(g)
    }
    return(p)
  }
}

#' gprofiler2.dotplot of enriched results
#'
#' @param object enrichResult or compareClusterResult object
#' @param by x axis, and order terms by this value
#' @param color color, one of pvalue
#' @param source DataSet source
#' @param showCategory number of terms to show
#' @param terms name of terms to show
#' @param font.size fontsize
#' @importFrom utils head
#' @importFrom dplyr bind_rows
#' @importFrom ggplot2 ggplot aes_string geom_point scale_color_continuous guide_colorbar ylab theme_bw theme element_text
#' @export
#'
publish_gostdot <- function(object, by = "precision", color = "p_value", source = NULL,
                               showCategory = 10, terms = NULL, font.size=12){

  if (dim(object$result)[1] != 0) {
    if (is.null(terms)) {
      results_df <- object$result[object$result$source == source, ]
      if (results_df$query %>% unique %>% length > 1) {
        results_df <- lapply(results_df$query %>% unique, function(x){
          data <- results_df[results_df$query == x, ]
          data <- data[order(data$p_value, decreasing = F), ] %>% head(showCategory)
          data <- data[order(data[ ,by], decreasing = T), ]
        }) %>% bind_rows()
      }else {
        results_df <- results_df[order(results_df$p_value, decreasing = F), ] %>% head(showCategory)
        results_df <- results_df[order(results_df[ ,by], decreasing = T), ]
      }
    }else {
      # results_df <- object$result[object$result$source == source, ]
      results_df <- object$result[object$result$term_name %in% terms, ]
      if (results_df$query %>% unique %>% length > 1) {
        results_df <- lapply(results_df$query %>% unique, function(x){
          data <- results_df[results_df$query == x, ]
          data <- data[order(data[ ,by], decreasing = T), ]
        }) %>% bind_rows()
      }else {
        # results_df <- results_df[results_df$term_name %in% terms, ]
        results_df <- results_df[order(results_df[ ,by], decreasing = T), ]
      }
    }
    results_df$term_name <- factor(results_df$term_name, levels = results_df$term_name %>% unique %>% rev)

    if (object$result$query %>% unique %>% length == 1) {
      plots <- ggplot(results_df, aes_string(x = by, y = "term_name", size = "intersection_size", color = color))+
        geom_point()+
        labs(x = "GeneRatio", y = NULL, size = "Gene Count", color = "P-value")+
        scale_color_continuous(low="red", high="blue", name = color, guide=guide_colorbar(reverse=TRUE))+
        ylab(NULL)+
        theme_bw()+
        theme(axis.text = element_text(size = font.size), text = element_text(size = font.size))
    }else {
      plots <- ggplot(results_df, aes_string(x = "query", y = "term_name", size = "intersection_size", color = color))+
        geom_point()+
        labs(x = "GeneRatio", y = NULL, size = "Gene Count", color = "P-value")+
        scale_color_continuous(low="red", high="blue", name = color, guide=guide_colorbar(reverse=TRUE))+
        theme_bw()+
        xlab(NULL) + ylab(NULL)+
        theme(axis.text = element_text(size = font.size),
              text = element_text(size = font.size),
              axis.text.x = element_text(angle = 45, hjust = 1))
    }
    return(plots)
  }
}


#' Plotting enriched results of gprofiler2, this function was modified from grofiler2's publish_gosttable function
#'
#' @param gostres enriched results of gprofiler2
#' @param highlight_terms terms to highlight
#' @param use_colors whether use color to highlight p-values
#' @param show_columns names of additional columns to show
#' @param filename file name to create on disk and save the annotated plot. Filename extension should be from c("png", "pdf", "jpeg", "tiff", "bmp")
#' @param ggplot whether use ggplot
#' @param fontsize fontsize
#' @param show_link show the link of gProfiler websit in the bottom
#' @import tidyr
#' @import grid
#' @import ggplot2
#' @import gridExtra
#' @importFrom gprofiler2 mapViridis
#' @importFrom graphics grid
#'
#' @export
#'
publish_gosttable2 <- function (gostres, highlight_terms = NULL, use_colors = TRUE, show_link = TRUE,
                                show_columns = c("source", "term_name", "term_size", "intersection_size"),
                                filename = NULL, ggplot = TRUE, fontsize = 15)
{
  term_id <- p_values <- query <- p_value <- NULL
  if (class(gostres) == "list") {
    if (!("result" %in% names(gostres)))
      stop("Name 'result' not found from the input")
    df <- gostres$result
  }
  else if (class(gostres) == "data.frame") {
    df <- gostres
  }
  else {
    stop("The input 'gostres' should be a data frame or a list from the gost() function.")
  }
  if (!"term_id" %in% colnames(df))
    stop("The required column 'term_id' is missing from the input")
  if (!any(grepl("p_value", colnames(df))))
    stop("Column 'p_value(s)' is missing from the input")
  if (is.null(highlight_terms)) {
    highlight_terms = df
  }
  if (is.data.frame(highlight_terms)) {
    message("The input 'highlight_terms' is a data.frame. The column 'term_id' will be used.")
    if ("term_id" %in% colnames(highlight_terms)) {
      highlight_terms <- highlight_terms$term_id
    }
    else {
      stop("No column named 'term_id'.")
    }
  }
  subdf <- base::subset(df, term_id %in% highlight_terms)
  if (nrow(subdf) == 0) {
    stop("None of the term IDs in the 'highlight_terms' were found from the results.")
  }
  highlight_terms <- unique(highlight_terms)
  subdf$id <- match(subdf$term_id, highlight_terms)
  subdf = subdf[order(subdf$id), ]
  show_columns <- unique(append(show_columns, c("id", "term_id",
                                                "p_value")))
  gp_colnames <- c("id", "source", "term_id", "term_name",
                   "term_size", "query_size", "intersection_size", "p_value",
                   "intersection_sizes", "query_sizes")
  colnames <- gp_colnames[which(gp_colnames %in% show_columns)]
  if (length(setdiff(show_columns, gp_colnames)) > 0) {
    colnames <- append(colnames, setdiff(show_columns, gp_colnames))
  }
  if ("p_values" %in% colnames(subdf)) {
    if ("meta" %in% names(gostres)) {
      meta <- gostres$meta
      subdf$query <- list(names(meta$query_metadata$queries))
    }
    else {
      qnames = paste("query", seq(1, length(subdf$p_values[[1]])),
                     sep = "_")
      subdf$query <- list(qnames)
    }
    spread_col = c("p_values")
    if ("query_sizes" %in% show_columns) {
      spread_col = append(spread_col, "query_sizes")
    }
    if ("intersection_sizes" %in% show_columns) {
      spread_col = append(spread_col, "intersection_sizes")
    }
    subdf <- tidyr::unnest(data = subdf, cols = c(spread_col,
                                                  query))
    subdf <- dplyr::rename(subdf, p_value = p_values)
    subdf$p_value <- formatC(subdf$p_value, format = "e",
                             digits = 1)
    showdf <- subdf[, stats::na.omit(match(c(colnames, "query"),
                                           names(subdf)))]
    showdf <- tidyr::pivot_wider(showdf, names_from = query,
                                 values_from = c(p_value, spread_col[spread_col !=
                                                                       "p_values"]), names_prefix = ifelse(length(spread_col) ==
                                                                                                             1, "p_value ", ""))
  }
  else {
    if ("query" %in% names(subdf) & length(unique(subdf$query)) >
        1) {
      subdf$p_value <- formatC(subdf$p_value, format = "e",
                               digits = 1)
      showdf <- subdf[, stats::na.omit(match(c(colnames,
                                               "query"), names(subdf)))]
      spread_col <- c("p_value", "intersection_size",
                      "query_size")
      spread_col <- intersect(colnames(showdf), spread_col)
      spread_col <- intersect(show_columns, spread_col)
      showdf <- tidyr::pivot_wider(showdf, names_from = query,
                                   values_from = spread_col, names_prefix = ifelse(length(spread_col) ==
                                                                                     1, "p_value ", ""))
      if ("meta" %in% names(gostres)) {
        input_order <- names(gostres$meta$query_metadata$queries)
        if (length(spread_col) == 1) {
          input_order <- paste("p_value", input_order)
        }
        else {
          input_order <- unlist(lapply(spread_col, function(x) paste(x,
                                                                     input_order, sep = "_")))
        }
        showdf <- showdf[c(names(showdf)[stats::na.omit(match(colnames,
                                                              names(showdf)))], input_order)]
      }
    }
    else {
      subdf$p_value <- formatC(subdf$p_value, format = "e",
                               digits = 1)
      showdf <- subdf[, stats::na.omit(match(colnames,
                                             names(subdf)))]
    }
  }
  idx <- which(grepl(pattern = "p_value", x = names(showdf)))
  if (use_colors) {
    order_of_cl = names(showdf)[idx]
    showdf[paste0(1, order_of_cl)] <- NA
    order_of_cl2 = c(rbind(paste0(1, order_of_cl), order_of_cl))
    showdf = showdf[, c(names(showdf)[1:min(idx) - 1], order_of_cl2)]
    colours <- matrix("white", nrow(showdf), ncol(showdf))
    temp_df = showdf[, order_of_cl2, drop = F]
    temp_cols <- sapply(temp_df, function(x) ifelse(!is.na(x),
                                                    mapViridis(-log10(as.numeric(x))), "white"))
    if (nrow(temp_df) == 1) {
      temp_cols = data.frame(t(temp_cols), check.names = F,
                             stringsAsFactors = F)
    }
    temp_cols[, seq(1, ncol(temp_cols), 2)] = temp_cols[,
                                                        seq(2, ncol(temp_cols), 2)]
    temp_cols[, seq(2, ncol(temp_cols), 2)] = "white"
    colours[, which(names(showdf) %in% order_of_cl2)] <- temp_cols
    if (nrow(temp_df) == 1) {
      colours = unlist(colours)
    }
    showdf[, startsWith(names(showdf), "1")] = ""
    names(showdf)[startsWith(names(showdf), "1")] = ""
  }
  else {
    colours <- matrix("white", nrow(showdf), ncol(showdf))
  }
  fontcolours <- matrix("black", nrow(showdf), ncol(showdf))
  fontfaces <- matrix("plain", nrow(showdf), ncol(showdf))
  th <- gridExtra::ttheme_default(base_size = fontsize, padding = grid::unit(c(8, 4), "mm"),
                                  core = list(padding.h = grid::unit(c(15, 15), "mm"),
                                              padding.v = grid::unit(c(15, 15), "mm"),
                                              bg_params = list(fill = colours, col = "black", lwd = 0.5),
                                              fg_params = list(hjust = 0, x = 0.02, col = fontcolours, fontface = fontfaces)),
                                  colhead = list(bg_params = list(fill = "gray99", lwd = 0.5, col = "black"),
                                                 fg_params = list(col = "gray39", fontface = "bold")),
                                  rowhead = list(fg_params = list(col = "black", fontface = "bold")))
  tb <- gridExtra::tableGrob(showdf, theme = th, rows = NULL)
  h <- grid::unit.c(sum(tb$heights))
  w <- grid::unit.c(sum(tb$widths))
  if (isTRUE(show_link)) {
    tg <- gridExtra::arrangeGrob(tb, ncol = 1, widths = w, heights = h,
                                 bottom = grid::textGrob("g:Profiler (biit.cs.ut.ee/gprofiler)",
                                                         x = 0.95, hjust = 1, gp = grid::gpar(fontsize = 10,
                                                                                       font = 8, col = "cornflowerblue")))
  }else {
    tg <- gridExtra::arrangeGrob(tb, ncol = 1, widths = w, heights = h,
                                 bottom = grid::textGrob("g:Profiler (biit.cs.ut.ee/gprofiler)",
                                                         x = 0.95, hjust = 1, gp = grid::gpar(fontsize = 0,
                                                                                              font = 8, col = "cornflowerblue")))
  }

  if (ggplot) {
    p <- ggplot2::ggplot() + ggplot2::annotation_custom(tg) +
      ggplot2::geom_blank() + ggplot2::theme_void()
  }
  p <- ggplot2::ggplot() + ggplot2::annotation_custom(tg) +
    ggplot2::geom_blank() + ggplot2::theme_void()
  if (is.null(filename)) {
    if (ggplot) {
      p <- ggplot2::ggplot() + ggplot2::annotation_custom(tg) +
        ggplot2::geom_blank() + ggplot2::theme_void()
      return(p)
    }
    else {
      return(tg)
    }
  }
  else {
    imgtype <- strsplit(basename(filename), split = "\\.")[[1]][-1]
    if (length(imgtype) == 0) {
      filename = paste0(filename, ".pdf")
    }
    if (tolower(imgtype) %in% c("png", "pdf", "jpeg", "tiff",
                                "bmp")) {
      width = grid::convertWidth(sum(tg$widths), "in",
                                 TRUE) + 0.2
      height = grid::convertHeight(sum(tg$heights), "in",
                                   TRUE) + 0.2
      p <- ggplot2::ggplot() + ggplot2::annotation_custom(tg) +
        ggplot2::geom_blank() + ggplot2::theme_void()
      ggplot2::ggsave(filename = filename, plot = p, height = height,
                      width = width)
      message("The image is saved to ", filename)
      return(p)
    }
    else {
      stop("The given file format is not supported.\nPlease use one of the following extensions: .png, .pdf, .jpeg, .tiff, .bmp")
    }
  }
}



#'  Plotting enriched results of gprofiler2, this function was modified from grofiler2's publish_gostplot function
#'
#' @param p ggplot object from gostplot
#' @param highlight_terms terms to highlight
#' @param filename filename to save
#' @param width plot width in inches
#' @param fontsize fontsize
#' @param height plot height in inches
#' @param show_columns additional coumns to show
#' @param show_link show the link of gProfiler websit in the bottom
#' @import tidyr
#' @import grid
#' @import ggplot2
#' @import gridExtra
#' @import grDevices
#'
#' @export
#'

publish_gostplot2 <- function(p, highlight_terms = NULL, filename = NULL, width = NA, fontsize = 14,
                               height = NA, show_columns = c("source", "term_name", "term_size"), show_link = TRUE)
{
  if (!("ggplot" %in% class(p))) {
    warning("Highlighting terms in a Manhattan plot is available for a ggplot object only.\nPlease set 'interactive = F' in the gostplot() function and try again.")
    return(NULL)
  }
  term_id <- logpval <- term_size_scaled <- id <- query <- p_value <- NULL
  if (!is.null(highlight_terms)) {
    if (is.data.frame(highlight_terms)) {
      message("The input 'highlight_terms' is a data.frame and therefore the column 'term_id' will be used for detection.")
      if ("term_id" %in% colnames(highlight_terms)) {
        highlight_terms <- highlight_terms$term_id
      }
      else {
        stop("No column named 'term_id'.")
      }
    }
    df <- p$data
    subdf <- base::subset(df, term_id %in% highlight_terms)
    if (nrow(subdf) == 0) {
      message("None of the term IDs in the 'highlight_terms' was found from the results.")
      return(p)
    }
    highlight_terms <- unique(highlight_terms)
    subdf$id <- match(subdf$term_id, highlight_terms)
    p <- p + ggplot2::geom_point(data = subdf, ggplot2::aes(x = order,
                                                            y = logpval, size = term_size_scaled), pch = 21,
                                 colour = "black")
    p <- p + ggplot2::geom_text(data = subdf, size = 4,colour = "white", ggplot2::aes(label = as.character(id),family = "mono", fontface = "bold"), hjust = -1.2,
                                vjust = -0.05) + ggplot2::geom_text(data = subdf, size = 4, colour = "black", fontface = "bold",
                                                                    ggplot2::aes(label = as.character(id)),hjust = -1.2, vjust = -0.05)+
      theme(text = element_text(size = fontsize), axis.title.y = element_text(size = fontsize), axis.text.x = element_text(size = fontsize))

    pseudo_gostres <- list(result = data.frame(subdf), meta = list(query_metadata = list(queries = sapply(unique(subdf$query),
                                                                                                          function(x) NULL))))
    tb <- publish_gosttable2(pseudo_gostres, highlight_terms = highlight_terms, fontsize = fontsize,
                            use_colors = TRUE, show_columns = show_columns, filename = NULL, ggplot = FALSE, show_link = show_link)
    h <- grid::unit.c(grid::unit(1, "null"), sum(tb$heights) +
                        grid::unit(3, "mm"))
    w <- grid::unit.c(grid::unit(1, "null"))
    tg <- gridExtra::grid.arrange(p, tb, ncol = 1, heights = h,
                                  widths = w, newpage = TRUE)
    p <- ggplot2::ggplot() + ggplot2::annotation_custom(tg) +
      ggplot2::geom_blank() + ggplot2::theme_void()
  }
  if (is.null(filename)) {
    return(p)
  }
  else {
    imgtype <- strsplit(basename(filename), split = "\\.")[[1]][-1]
    if (length(imgtype) == 0) {
      filename = paste0(filename, ".pdf")
    }
    if (tolower(imgtype) %in% c("png", "pdf", "jpeg", "tiff",
                                "bmp")) {
      if (is.na(width)) {
        width = max(grDevices::dev.size()[1], 8)
      }
      if (is.na(height)) {
        height = max(grDevices::dev.size()[2], 6)
      }
      ggplot2::ggsave(filename = filename, plot = p, width = width,
                      height = height, limitsize = F)
      message("The image is saved to ", filename)
      return(p)
    }
    else {
      stop("The given file format is not supported.\nPlease use one of the following extensions: .png, .pdf, .jpeg, .tiff, .bmp")
    }
  }
}

#'  ridgeplot for gsea enriched results (modified from enrichplot::ridgeplot)
#'
#' @param x gseaResults object
#' @param showCategory top n most significant terms
#' @param fill color to fill the density plot
#' @param core_enrichment whether only using core_enriched genes
#' @param label_format a numeric value sets wrap length, alternatively a custom function to format axis labels. by default wraps names longer that 30 characters
#' @param orderBy order the terms by
#' @param decreasing logical
#' @param terms term ID that of interest
#' @import DOSE
#' @import ggplot2
#' @import enrichplot
#'
#' @export
#'

ridgeplot2 <- function(x = NULL, showCategory=30, fill="p.adjust",
                       core_enrichment = TRUE, label_format = 300, orderBy = "NES", decreasing = FALSE, terms = NULL) {
  if (!is(x, "gseaResult"))
    stop("currently only support gseaResult")
  
  if (fill == "qvalue") {
    fill <- "qvalues"
  }
  if (!fill %in% colnames(x@result)) {
    stop("'fill' variable not available ...")
  }
  
  if (orderBy !=  'NES' && !orderBy %in% colnames(x@result)) {
    message('wrong orderBy parameter; set to default `orderBy = "NES"`')
    orderBy <- "NES"
  }
  
  if (length(terms) != 0) {
    n <- as.data.frame(x) %>% nrow()
  }else {
    n <- showCategory
  }
  
  if (core_enrichment) {
    gs2id <- DOSE::geneInCategory(x)[seq_len(n)]
  } else {
    gs2id <- x@geneSets[x$ID[seq_len(n)]]
  }
  
  if (x@readable) {
    id <- match(names(x@geneList), names(x@gene2Symbol))
    names(x@geneList) <- x@gene2Symbol[id]
  } 
  
  gs2val <- lapply(gs2id, function(id) {
    res <- x@geneList[id]
    res <- res[!is.na(res)]
  })
  
  if (length(terms) != 0) {
    id <- as.data.frame(x)[as.data.frame(x)$Description %in% terms, "ID"]
    gs2val <- gs2val[id]
  }
  
  nn <- names(gs2val)
  i <- match(nn, x$ID)
  nn <- x$Description[i]
  
  j <- order(x@result[[orderBy]][i], decreasing = decreasing)
  len <- sapply(gs2val, length)
  gs2val.df <- data.frame(category = rep(nn, times=len),
                          color = rep(x[i, fill], times=len),
                          value = unlist(gs2val))
  
  colnames(gs2val.df)[2] <- fill
  gs2val.df$category <- factor(gs2val.df$category, levels=nn[j])
  
  label_func <- enrichplot:::default_labeller(label_format)
  if(is.function(label_format)) {
    label_func <- label_format
  }
  
  ggplot(gs2val.df, aes_string(x="value", y="category", fill=fill)) +
    ggridges::geom_density_ridges() +
    scale_fill_continuous(low="red", high="blue", name = fill,
                          guide=guide_colorbar(reverse=TRUE)) +
    scale_y_discrete(labels = label_func) +
    xlab(NULL) + ylab(NULL) +  theme_dose()
}


#'  generate gene network graph of GENIE3 (modified from GGally::ggnet2)
#'
#'@description Fixed a problem where labels are not displayed after plotly::ggplotly conversion
#' @export
#'

ggnet3 <- function (net, mode = "fruchtermanreingold", layout.par = NULL, 
                    layout.exp = 0, alpha = 1, color = "grey75", shape = 19, 
                    size = 9, max_size = 9, na.rm = NA, palette = NULL, alpha.palette = NULL, 
                    alpha.legend = NA, color.palette = palette, color.legend = NA, 
                    shape.palette = NULL, shape.legend = NA, size.palette = NULL, 
                    size.legend = NA, size.zero = FALSE, size.cut = FALSE, size.min = NA, 
                    size.max = NA, label = FALSE, label.alpha = 1, label.color = "black", 
                    label.size = max_size/2, label.trim = FALSE, node.alpha = alpha, 
                    node.color = color, node.label = label, node.shape = shape, 
                    node.size = size, edge.alpha = 1, edge.color = "grey50", 
                    edge.lty = "solid", edge.size = 0.25, edge.label = NULL, 
                    edge.label.alpha = 1, edge.label.color = label.color, edge.label.fill = "white", 
                    edge.label.size = max_size/2, arrow.size = 0, arrow.gap = 0, 
                    arrow.type = "closed", legend.size = 9, legend.position = "right", 
                    ...) 
{
  GGally:::require_namespaces(c("network", "sna", "scales"))
  if (inherits(net, "igraph") && "intergraph" %in% rownames(installed.packages())) {
    net = intergraph::asNetwork(net)
  }
  else if (inherits(net, "igraph")) {
    stop("install the 'intergraph' package to use igraph objects with ggnet2")
  }
  if (!network::is.network(net)) {
    net = try(network::network(net), silent = TRUE)
  }
  if (!network::is.network(net)) {
    stop("could not coerce net to a network object")
  }
  get_v = get("%v%", envir = getNamespace("network"))
  get_e = get("%e%", envir = getNamespace("network"))
  set_mode = function(x, mode = network::get.network.attribute(x, 
                                                               "bipartite")) {
    c(rep("actor", mode), rep("event", n_nodes - mode))
  }
  set_node = function(x, value, mode = TRUE) {
    if (is.null(x) || any(is.na(x)) || any(is.infinite(x)) || 
        any(is.nan(x))) {
      stop(paste("incorrect", value, "value"))
    }
    else if (is.numeric(x) && any(x < 0)) {
      stop(paste("incorrect", value, "value"))
    }
    else if (length(x) == n_nodes) {
      x
    }
    else if (length(x) > 1) {
      stop(paste("incorrect", value, "length"))
    }
    else if (any(x %in% v_attr)) {
      get_v(net, x)
    }
    else if (mode && identical(x, "mode") && is_bip) {
      set_mode(net)
    }
    else {
      x
    }
  }
  set_edge = function(x, value) {
    if (is.null(x) || any(is.na(x)) || any(is.infinite(x)) || 
        any(is.nan(x))) {
      stop(paste("incorrect", value, "value"))
    }
    else if (is.numeric(x) && any(x < 0)) {
      stop(paste("incorrect", value, "value"))
    }
    else if (length(x) == n_edges) {
      x
    }
    else if (length(x) > 1) {
      stop(paste("incorrect", value, "length"))
    }
    else if (any(x %in% e_attr)) {
      get_e(net, x)
    }
    else {
      x
    }
  }
  set_attr = function(x) {
    if (length(x) == n_nodes) {
      x
    }
    else if (length(x) > 1) {
      stop(paste("incorrect coordinates length"))
    }
    else if (!x %in% v_attr) {
      stop(paste("vertex attribute", x, "was not found"))
    }
    else if (!is.numeric(get_v(net, x))) {
      stop(paste("vertex attribute", x, "is not numeric"))
    }
    else {
      get_v(net, x)
    }
  }
  set_name = function(x, y) {
    z = length(x) == 1 && x %in% v_attr
    z = ifelse(is.na(y), z, y)
    z = ifelse(isTRUE(z), x, z)
    ifelse(is.logical(z), "", z)
  }
  set_size = function(x) {
    y = x + (0 %in% x) * !size.zero
    y = scales::rescale_max(y)
    y = (scales::abs_area(max_size))(y)
    if (is.null(names(x))) 
      names(y) = x
    else names(y) = names(x)
    y
  }
  is_one = function(x) length(unique(x)) == 1
  is_col = function(x) all(is.numeric(x)) | all(network::is.color(x))
  n_nodes = network::network.size(net)
  n_edges = network::network.edgecount(net)
  v_attr = network::list.vertex.attributes(net)
  e_attr = network::list.edge.attributes(net)
  is_bip = network::is.bipartite(net)
  is_dir = ifelse(network::is.directed(net), "digraph", "graph")
  if (!is.numeric(arrow.size) || arrow.size < 0) {
    stop("incorrect arrow.size value")
  }
  else if (arrow.size > 0 & is_dir == "graph") {
    warning("network is undirected; arrow.size ignored")
    arrow.size = 0
  }
  if (!is.numeric(arrow.gap) || arrow.gap < 0 || arrow.gap > 
      1) {
    stop("incorrect arrow.gap value")
  }
  else if (arrow.gap > 0 & is_dir == "graph") {
    warning("network is undirected; arrow.gap ignored")
    arrow.gap = 0
  }
  if (network::is.hyper(net)) {
    stop("ggnet2 cannot plot hyper graphs")
  }
  if (network::is.multiplex(net)) {
    stop("ggnet2 cannot plot multiplex graphs")
  }
  if (network::has.loops(net)) {
    warning("ggnet2 does not know how to handle self-loops")
  }
  x = max_size
  if (!is.numeric(x) || is.infinite(x) || is.nan(x) || x < 
      0) {
    stop("incorrect max_size value")
  }
  data = data.frame(label = get_v(net, "vertex.names"), stringsAsFactors = FALSE)
  data$alpha = set_node(node.alpha, "node.alpha")
  data$color = set_node(node.color, "node.color")
  data$shape = set_node(node.shape, "node.shape")
  data$size = set_node(node.size, "node.size")
  if (length(na.rm) > 1) {
    stop("incorrect na.rm value")
  }
  else if (!is.na(na.rm)) {
    if (!na.rm %in% v_attr) {
      stop(paste("vertex attribute", na.rm, "was not found"))
    }
    x = which(is.na(get_v(net, na.rm)))
    message(paste("na.rm removed", length(x), "nodes out of", 
                  nrow(data)))
    if (length(x) > 0) {
      data = data[-x, ]
      network::delete.vertices(net, x)
      if (!nrow(data)) {
        warning("na.rm removed all nodes; nothing left to plot")
        return(invisible(NULL))
      }
    }
  }
  x = size
  if (length(x) == 1 && x %in% c("indegree", "outdegree", 
                                 "degree", "freeman")) {
    if ("package:igraph" %in% search()) {
      y = ifelse(is_dir == "digraph", "directed", "undirected")
      z = c(indegree = "in", outdegree = "out", degree = "all", 
            freeman = "all")[x]
      data$size = igraph::degree(igraph::graph.adjacency(as.matrix(net), 
                                                         mode = y), mode = z)
    }
    else {
      data$size = sna::degree(net, gmode = is_dir, cmode = ifelse(x == 
                                                                    "degree", "freeman", x))
    }
    size.legend = ifelse(is.na(size.legend), x, size.legend)
  }
  x = ifelse(is.na(size.min), 0, size.min)
  if (length(x) > 1 || !is.numeric(x) || is.infinite(x) || 
      is.nan(x) || x < 0) {
    stop("incorrect size.min value")
  }
  else if (x > 0 && !is.numeric(data$size)) {
    warning("node.size is not numeric; size.min ignored")
  }
  else if (x > 0) {
    x = which(data$size < x)
    message(paste("size.min removed", length(x), "nodes out of", 
                  nrow(data)))
    if (length(x) > 0) {
      data = data[-x, ]
      network::delete.vertices(net, x)
      if (!nrow(data)) {
        warning("size.min removed all nodes; nothing left to plot")
        return(invisible(NULL))
      }
    }
  }
  x = ifelse(is.na(size.max), 0, size.max)
  if (length(x) > 1 || !is.numeric(x) || is.infinite(x) || 
      is.nan(x) || x < 0) {
    stop("incorrect size.max value")
  }
  else if (x > 0 && !is.numeric(data$size)) {
    warning("node.size is not numeric; size.max ignored")
  }
  else if (x > 0) {
    x = which(data$size > x)
    message(paste("size.max removed", length(x), "nodes out of", 
                  nrow(data)))
    if (length(x) > 0) {
      data = data[-x, ]
      network::delete.vertices(net, x)
      if (!nrow(data)) {
        warning("size.max removed all nodes; nothing left to plot")
        return(invisible(NULL))
      }
    }
  }
  x = size.cut
  if (length(x) > 1 || is.null(x) || is.na(x) || is.infinite(x) || 
      is.nan(x)) {
    stop("incorrect size.cut value")
  }
  else if (isTRUE(x)) {
    x = 4
  }
  else if (is.logical(x) && !x) {
    x = 0
  }
  else if (!is.numeric(x)) {
    stop("incorrect size.cut value")
  }
  if (x >= 1 && !is.numeric(data$size)) {
    warning("node.size is not numeric; size.cut ignored")
  }
  else if (x >= 1) {
    x = unique(quantile(data$size, probs = seq(0, 1, by = 1/as.integer(x))))
    if (length(x) > 1) {
      data$size = cut(data$size, unique(x), include.lowest = TRUE)
    }
    else {
      warning("node.size is invariant; size.cut ignored")
    }
  }
  if (!is.null(alpha.palette)) {
    x = alpha.palette
  }
  else if (is.factor(data$alpha)) {
    x = levels(data$alpha)
  }
  else {
    x = unique(data$alpha)
  }
  if (!is.null(names(x))) {
    y = unique(na.omit(data$alpha[!data$alpha %in% names(x)]))
    if (length(y) > 0) {
      stop(paste("no alpha.palette value for", paste0(y, 
                                                      collapse = ", ")))
    }
  }
  else if (is.factor(data$alpha) || !is.numeric(x)) {
    data$alpha = factor(data$alpha)
    x = scales::rescale_max(1:length(levels(data$alpha)))
    names(x) = levels(data$alpha)
  }
  alpha.palette = x
  if (!is.null(color.palette)) {
    x = color.palette
  }
  else if (is.factor(data$color)) {
    x = levels(data$color)
  }
  else {
    x = unique(data$color)
  }
  if (length(x) == 1 && "RColorBrewer" %in% rownames(installed.packages()) && 
      x %in% rownames(RColorBrewer::brewer.pal.info)) {
    data$color = factor(data$color)
    n_groups = length(levels(data$color))
    n_colors = RColorBrewer::brewer.pal.info[x, "maxcolors"]
    if (n_groups > n_colors) {
      stop(paste0("too many node groups (", n_groups, 
                  ") for ", "ColorBrewer palette ", x, " (max: ", 
                  n_colors, ")"))
    }
    else if (n_groups < 3) {
      n_groups = 3
    }
    x = RColorBrewer::brewer.pal(n_groups, x)[1:length(levels(data$color))]
    names(x) = levels(data$color)
  }
  if (!is.null(names(x))) {
    y = unique(na.omit(data$color[!data$color %in% names(x)]))
    if (length(y) > 0) {
      stop(paste("no color.palette value for", paste0(y, 
                                                      collapse = ", ")))
    }
  }
  else if (is.factor(data$color) || !is_col(x)) {
    data$color = factor(data$color)
    x = gray.colors(length(x))
    names(x) = levels(data$color)
  }
  color.palette = x
  if (!is.null(shape.palette)) {
    x = shape.palette
  }
  else if (is.factor(data$shape)) {
    x = levels(data$shape)
  }
  else {
    x = unique(data$shape)
  }
  if (!is.null(names(x))) {
    y = unique(na.omit(data$shape[!data$shape %in% names(x)]))
    if (length(y) > 0) {
      stop(paste("no shape.palette value for", paste0(y, 
                                                      collapse = ", ")))
    }
  }
  else if (is.factor(data$shape) || !is.numeric(x)) {
    data$shape = factor(data$shape)
    x = (scales::shape_pal())(length(levels(data$shape)))
    names(x) = levels(data$shape)
  }
  shape.palette = x
  if (!is.null(size.palette)) {
    x = size.palette
  }
  else if (is.factor(data$size)) {
    x = levels(data$size)
  }
  else {
    x = unique(data$size)
  }
  if (!is.null(names(x))) {
    y = unique(na.omit(data$size[!data$size %in% names(x)]))
    if (length(y) > 0) {
      stop(paste("no size.palette value for", paste0(y, 
                                                     collapse = ", ")))
    }
  }
  else if (is.factor(data$size) || !is.numeric(x)) {
    data$size = factor(data$size)
    x = 1:length(levels(data$size))
    names(x) = levels(data$size)
  }
  size.palette = x
  l = node.label
  if (isTRUE(l)) {
    l = data$label
  }
  else if (length(l) > 1 & length(l) == n_nodes) {
    data$label = l
  }
  else if (length(l) == 1 && l %in% v_attr) {
    l = get_v(net, l)
  }
  else {
    l = ifelse(data$label %in% l, data$label, "")
  }
  if (is.character(mode) && length(mode) == 1) {
    mode = paste0("gplot.layout.", mode)
    if (!exists(mode, where = getNamespace("sna"))) {
      stop(paste("unsupported placement method:", mode))
    }
    else {
      mode <- get(mode, getNamespace("sna"))
    }
    xy = network::as.matrix.network.adjacency(net)
    xy = do.call(mode, list(xy, layout.par))
    xy = data.frame(x = xy[, 1], y = xy[, 2])
  }
  else if (is.character(mode) && length(mode) == 2) {
    xy = data.frame(x = set_attr(mode[1]), y = set_attr(mode[2]))
  }
  else if (is.numeric(mode) && is.matrix(mode)) {
    xy = data.frame(x = set_attr(mode[, 1]), y = set_attr(mode[, 
                                                               2]))
  }
  else {
    stop("incorrect mode value")
  }
  xy$x = scale(xy$x, min(xy$x), diff(range(xy$x)))[, 1]
  xy$y = scale(xy$y, min(xy$y), diff(range(xy$y)))[, 1]
  data = cbind(data, xy)
  edges = network::as.matrix.network.edgelist(net)
  if (edge.color[1] == "color" && length(edge.color) == 2) {
    edge.color = ifelse(data$color[edges[, 1]] == data$color[edges[, 
                                                                   2]], as.character(data$color[edges[, 1]]), edge.color[2])
    if (!is.null(names(color.palette))) {
      x = which(edge.color %in% names(color.palette))
      edge.color[x] = color.palette[edge.color[x]]
    }
    edge.color[is.na(edge.color)] = edge.color[2]
  }
  edge.color = set_edge(edge.color, "edge.color")
  if (!is_col(edge.color)) {
    stop("incorrect edge.color value")
  }
  edges = data.frame(xy[edges[, 1], ], xy[edges[, 2], ])
  names(edges) = c("X1", "Y1", "X2", "Y2")
  if (!is.null(edge.label)) {
    edges$midX = (edges$X1 + edges$X2)/2
    edges$midY = (edges$Y1 + edges$Y2)/2
    edges$label = set_edge(edge.label, "edge.label")
    edge.label.alpha = set_edge(edge.label.alpha, "edge.label.alpha")
    if (!is.numeric(edge.label.alpha)) {
      stop("incorrect edge.label.alpha value")
    }
    edge.label.color = set_edge(edge.label.color, "edge.label.color")
    if (!is_col(edge.label.color)) {
      stop("incorrect edge.label.color value")
    }
    edge.label.size = set_edge(edge.label.size, "edge.label.size")
    if (!is.numeric(edge.label.size)) {
      stop("incorrect edge.label.size value")
    }
  }
  edge.lty = set_edge(edge.lty, "edge.lty")
  edge.size = set_edge(edge.size, "edge.size")
  if (!is.numeric(edge.size) || any(edge.size <= 0)) {
    stop("incorrect edge.size value")
  }
  p = ggplot(data, aes(x = x, y = y, label = label))
  if (nrow(edges) > 0) {
    if (arrow.gap > 0) {
      x.dir = with(edges, (X2 - X1))
      y.dir = with(edges, (Y2 - Y1))
      arrow.gap = with(edges, arrow.gap/sqrt(x.dir^2 + 
                                               y.dir^2))
      edges = transform(edges, X1 = X1 + arrow.gap * x.dir, 
                        Y1 = Y1 + arrow.gap * y.dir, X2 = X1 + (1 - 
                                                                  arrow.gap) * x.dir, Y2 = Y1 + (1 - arrow.gap) * 
                          y.dir)
    }
    p = p + geom_segment(data = edges, aes(x = X1, y = Y1, 
                                           xend = X2, yend = Y2), size = edge.size, color = edge.color, 
                         alpha = edge.alpha, lty = edge.lty, arrow = arrow(type = arrow.type, 
                                                                           length = unit(arrow.size, "pt")))
  }
  if (nrow(edges) > 0 && !is.null(edge.label)) {
    p = p + geom_point(data = edges, aes(x = midX, y = midY), 
                       alpha = edge.alpha, color = edge.label.fill, size = edge.label.size * 
                         1.5) + geom_text(data = edges, aes(x = midX, 
                                                            y = midY, label = label), alpha = edge.label.alpha, 
                                          color = edge.label.color, size = edge.label.size)
  }
  x = list()
  if (is.numeric(data$alpha) && is_one(data$alpha)) {
    x = c(x, alpha = unique(data$alpha))
  }
  if (!is.factor(data$color) && is_one(data$color)) {
    x = c(x, colour = unique(data$color))
  }
  if (is.numeric(data$shape) && is_one(data$shape)) {
    x = c(x, shape = unique(data$shape))
  }
  if (is.numeric(data$size) && is_one(data$size)) {
    x = c(x, size = unique(data$size))
  }
  else {
    x = c(x, size = max_size)
  }
  p = p + geom_point(aes(alpha = factor(alpha), color = factor(color), 
                         shape = factor(shape), size = factor(size)))
  if (is.numeric(data$alpha)) {
    v_alpha = unique(data$alpha)
    names(v_alpha) = unique(data$alpha)
    p = p + scale_alpha_manual("", values = v_alpha) + guides(alpha = FALSE)
  }
  else {
    p = p + scale_alpha_manual(set_name(node.alpha, alpha.legend), 
                               values = alpha.palette, breaks = names(alpha.palette), 
                               guide = guide_legend(override.aes = x))
  }
  if (!is.null(names(color.palette))) {
    p = p + scale_color_manual(set_name(node.color, color.legend), 
                               values = color.palette, breaks = names(color.palette), 
                               guide = guide_legend(override.aes = x))
  }
  else {
    v_color = unique(data$color)
    names(v_color) = unique(data$color)
    p = p + scale_color_manual("", values = v_color) + guides(color = FALSE)
  }
  if (is.numeric(data$shape)) {
    v_shape = unique(data$shape)
    names(v_shape) = unique(data$shape)
    p = p + scale_shape_manual("", values = v_shape) + guides(shape = FALSE)
  }
  else {
    p = p + scale_shape_manual(set_name(node.shape, shape.legend), 
                               values = shape.palette, breaks = names(shape.palette), 
                               guide = guide_legend(override.aes = x))
  }
  x = x[names(x) != "size"]
  if (is.numeric(data$size)) {
    v_size = set_size(unique(data$size))
    if (length(v_size) == 1) {
      v_size = as.numeric(names(v_size))
      p = p + scale_size_manual("", values = v_size) + 
        guides(size = FALSE)
    }
    else {
      p = p + scale_size_manual(set_name(node.size, size.legend), 
                                values = v_size, guide = guide_legend(override.aes = x))
    }
  }
  else {
    p = p + scale_size_manual(set_name(node.size, size.legend), 
                              values = set_size(size.palette), guide = guide_legend(override.aes = x))
  }
  if (!is_one(l) || unique(l) != "") {
    label.alpha = set_node(label.alpha, "label.alpha", mode = FALSE)
    if (!is.numeric(label.alpha)) {
      stop("incorrect label.alpha value")
    }
    label.color = set_node(label.color, "label.color", mode = FALSE)
    if (!is_col(label.color)) {
      stop("incorrect label.color value")
    }
    label.size = set_node(label.size, "label.size", mode = FALSE)
    if (!is.numeric(label.size)) {
      stop("incorrect label.size value")
    }
    x = label.trim
    if (length(x) > 1 || (!is.logical(x) & !is.numeric(x) & 
                          !is.function(x))) {
      stop("incorrect label.trim value")
    }
    else if (is.numeric(x) && x > 0) {
      l = substr(l, 1, x)
    }
    else if (is.function(x)) {
      l = x(l)
    }
    p = p + geom_text(label = l, alpha = label.alpha, color = label.color, 
                      size = label.size, ...)
  }
  x = range(data$x)
  if (!is.numeric(layout.exp) || layout.exp < 0) {
    stop("incorrect layout.exp value")
  }
  else if (layout.exp > 0) {
    x = scales::expand_range(x, layout.exp/2)
  }
  p = p + scale_x_continuous(breaks = NULL, limits = x) + 
    scale_y_continuous(breaks = NULL) + theme(panel.background = element_blank(), 
                                              panel.grid = element_blank(), axis.title = element_blank(), 
                                              legend.key = element_blank(), legend.position = legend.position, 
                                              legend.text = element_text(size = legend.size), legend.title = element_text(size = legend.size))
  return(p)
}
