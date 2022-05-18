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

  results_df <- fortify_results(model = object, showCategory = showCategory, by = size, terms = terms)

  if (is.null(results_df)) {
    return(NULL)
  }else {
    if (inherits(object, "enrichResult")) {
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

