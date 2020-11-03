#' convert enrichResult or compareClusterResult to a data.frame that ready for plot
#'
#' @param model 'enrichResult' or 'compareClusterResult' object
#' @param showCategory Category numbers to show
#' @param terms enriched terms to show
#' @param by one of Count and GeneRatio
#' @importFrom DOSE parse_ratio
#'
#'
#' @return data.frame
#' @export
#'

fortify_results <- function(model, showCategory = 10, terms = NULL, by="geneRatio") {

  if (by == "geneRatio" | by == "GeneRatio") {
    by = "geneRatio"
  }

  if (dim(as.data.frame(model))[1] != 0) {
    if (inherits(model, "compareClusterResult")) {
      if (is.null(terms)) {
        clsp <- loadNamespace("clusterProfiler")
        result <- clsp$fortify.compareClusterResult(model, showCategory = showCategory, by = by)
        # result <- fortify(model, showCategory = showCategory, by = by)
      }else {
        result <- as.data.frame(model)
        result <- result[result$Description %in% terms, ]

        ## remove zero count
        result$Description <- as.character(result$Description) ## un-factor
        GOlevel <- result[,c("ID", "Description")] ## GO ID and Term
        GOlevel <- unique(GOlevel)

        result <- result[result$Count != 0, ]
        result$Description <- factor(result$Description,
                                     levels=rev(GOlevel[,2]))

        gsize <- as.numeric(sub("/\\d+$", "", as.character(result$GeneRatio)))
        gcsize <- as.numeric(sub("^\\d+/", "", as.character(result$GeneRatio)))
        result$GeneRatio = gsize/gcsize
        cluster <- paste(as.character(result$Cluster),"\n", "(", gcsize, ")", sep="")
        lv <- unique(cluster)[order(as.numeric(unique(result$Cluster)))]
        result$Cluster <- factor(cluster, levels = lv)
      }
    }else if (inherits(model, "enrichResult")) {
      if (is.null(terms)) {
        enrp <- loadNamespace("enrichplot")
        result <- enrp$fortify.enrichResult(model, showCategory = showCategory, by = by, order = T)
        # result <- fortify(model, showCategory = showCategory, by = by, order = T)
      }else {
        result <- as.data.frame(model)

        result <- result[result$Description %in% terms, ]

        result$GeneRatio <- parse_ratio(result$GeneRatio)

        if (by == "Count") {
          idx <- order(result$Count, decreasing=TRUE)
        }else {
          idx <- order(result$GeneRatio, decreasing=TRUE)
        }
        result <- result[idx,]

        result$Description <- factor(result$Description,
                                     levels=rev(unique(result$Description)))
      }
    }
    return(result)
  }else {
    warning("no terms enriched ...")
  }
}
