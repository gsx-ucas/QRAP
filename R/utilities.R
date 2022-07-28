#'  Auto set the annotation colors for pheatmap
#'
#' @param anno_row dataframe for annotate row colors
#' @param anno_col dataframe for annotate col colors
#'
#' @export

set_anno_color <- function(anno_row, anno_col) {
  require(RColorBrewer)
  annocolor <- c(as.list(anno_row), as.list(anno_col))

  annocolor <- lapply(annocolor,function(x){if(is.factor(x)){x=levels(x);return(x)}else{x=unique(x);return(x)}})

  qual_color_df <- brewer.pal.info[brewer.pal.info$category == "qual", ]
  qual_idx <- lapply(1:length(annocolor), function(x){if (is.character(annocolor[[x]])) {return(x)} else {return(NULL)}}) %>% unlist()
  if (!is.null(qual_idx)) {
    qual_annocolor <- annocolor[qual_idx]

    if (length(qual_annocolor) > nrow(qual_color_df)) {
      loop_seq <- nrow(qual_color_df)
    }else {
      loop_seq <- length(qual_annocolor)
    }

    qual_annocolor_list <- lapply(1:loop_seq, function(x){
      len_charater <- qual_annocolor[[x]] %>% length()
      qual_color <- qual_color_df[x, ]
      if (qual_color$maxcolors >= len_charater) {
        if (len_charater < 3) {
          colors <- brewer.pal(3,rownames(qual_color))
          colors <- colors[1:len_charater]
        }else {
          colors <- brewer.pal(len_charater,rownames(qual_color))
        }
      }else {
        colors <- colorRampPalette(brewer.pal(qual_color$maxcolors, rownames(qual_color)))(len_charater)
      }
      names(colors) <- qual_annocolor[[x]]
      return(colors)
    })

    names(qual_annocolor_list) <- names(qual_annocolor[1:loop_seq])
  }else {
    qual_annocolor_list <- NULL
  }


  seq_color_df <- brewer.pal.info[brewer.pal.info$category == "seq", ]
  seq_idx <- lapply(1:length(annocolor), function(x){if (is.numeric(annocolor[[x]])) {return(x)} else {return(NULL)}}) %>% unlist()
  if (!is.null(seq_idx)) {
    seq_annocolor <- annocolor[seq_idx]

    if (length(seq_annocolor) > nrow(seq_color_df)) {
      loop_seq <- nrow(seq_color_df)
    }else {
      loop_seq <- length(seq_annocolor)
    }

    seq_annocolor_list <- lapply(1:loop_seq, function(x){
      len_num <- seq_annocolor[[x]] %>% length()
      seq_color <- seq_color_df[x, ]
      if (seq_color$maxcolors >= len_num) {
        if (len_num < 3) {
          colors <- brewer.pal(3,rownames(seq_color))
          colors <- colors[1:len_num]
        }else {
          colors <- brewer.pal(len_num,rownames(seq_color))
        }
      }else {
        colors <- colorRampPalette(brewer.pal(seq_color$maxcolors, rownames(seq_color)))(len_num)
      }
      names(colors) <- seq_annocolor[[x]] %>% sort(na.last = FALSE)
      return(colors)
    })

    names(seq_annocolor_list) <- names(seq_annocolor[1:loop_seq])
  }else {
    seq_annocolor_list <- NULL
  }

  annocolor_list <- c(qual_annocolor_list, seq_annocolor_list)

  return(annocolor_list)
}



#' Get A List Of Differentially Expressed Genes
#'
#' @param dds object of produced by DESeq
#' @param ctrl Control group
#' @param treat TreatMent groups
#' @param p.adjust Adjusted P-values
#' @param abs.lfc Log2 FoldChange Abs values
#' @param save save DEG List to Local Directory
#' @importFrom DESeq2 results
#'
#' @return DEG List
#' @export
#'

get.DEGs <- function(dds, ctrl, treat, p.adjust = 0.05, abs.lfc = 1, save = FALSE) {
  if (length(treat) == 0) {
    stop("Treatment groups can not be null ...", call. = FALSE)
  }
  DEGs.List <- lapply(treat, function(x){
    Res <- as.data.frame(results(dds, contrast = c("condition", x, ctrl)))
    Des <- subset(Res, padj < p.adjust & abs(log2FoldChange) > abs.lfc)
    if (save == TRUE) {
      if (!dir.exists("REGs")) {
        dir.create("REGs")
      }
      write.csv(Res, paste0("REGs/", x, "_vs_", ctrl, ".csv"))
      
      if (!dir.exists("DEGs")) {
        dir.create("DEGs")
      }
      write.csv(Des, paste0("DEGs/", x, "_vs_", ctrl, ".csv"))
    }
    return(Res)
  })
  names(DEGs.List) <- paste(treat, ctrl, sep = "_vs_")
  return(DEGs.List)
}


#' Loadding DEGs from local directory
#'
#' @param filesName FileNames
#'
#' @return DEG List
#' @export
#'
load.DEGs <- function(filesName) {
  DEGs.files <- paste0("DEGs/", filesName, ".csv")
  DEGs.List <- lapply(DEGs.files, function(x){
    read.csv(x, row.names = 1, header = T)
  })
  names(DEGs.List) <- filesName
  return(DEGs.List)
}

#' Loadding REGs from local directory
#'
#' @param filesName FileNames
#'
#' @return REG List
#' @export
#'

load.REGs <- function(filesName) {
  REGs.files <- paste0("REGs/", filesName, ".csv")
  REGs.List <- lapply(REGs.files, function(x){
    read.csv(x, row.names = 1, header = T)
  })
  names(REGs.List) <- filesName
  return(REGs.List)
}



#' Subset The Experiment design Table and keep the order
#'
#' @param dds object of produced by DESeq
#' @param vars columns names
#' @param selected variable to keep
#'
#' @return Data.Frame
#' @export
#'

subset_Tab <- function(dds, vars, selected = NULL) {
  sampleTable <- as.data.frame(SummarizedExperiment::colData(dds))
  rownames(sampleTable) <- sampleTable$samples
  if (!is.null(selected)) {
    idx <- lapply(selected, function(x){
      sampleTable[sampleTable[, vars] == x, "samples"]
    }) %>% unlist
    idx <- idx[idx %in% rownames(sampleTable)]
    sampleTable <- sampleTable[idx, ]
  }
  return(sampleTable)
}

