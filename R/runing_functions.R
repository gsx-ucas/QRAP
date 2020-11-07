#' Prepare and Run DESeq
#'
#' @param expr.data expression matrix
#' @param sample.data sample infomation meta data
#' @param formula experiment design formula
#' @param test either "Wald" or "LRT", which will then use either Wald significance tests (defined by nbinomWaldTest),
#' or the likelihood ratio test on the difference in deviance between a full and reduced model formula (defined by nbinomLRT)
#' @param fitType either "parametric", "local", or "mean" for the type of fitting of dispersions to the mean intensity.
#' @param sfType either "ratio", "poscounts", or "iterate" for teh type of size factor estimation.
#' @param betaPrior whether or not to put a zero-mean normal prior on the non-intercept coefficients
#' @param reduced for test="LRT", a reduced formula to compare against
#' @param minReplicatesForReplace the minimum number of replicates required in order to use replaceOutliers on a sample.
#' @param modelMatrixType either "standard" or "expanded", which describe how the model matrix, X of the GLM formula is formed.
#' @param useT logical, passed to nbinomWaldTest, default is FALSE, where Wald statistics are assumed to follow a standard Normal
#' @param minmu lower bound on the estimated count for fitting genewise dispersion and for use with nbinomWaldTest and nbinomLRT
#' @importFrom DESeq2 DESeq DESeqDataSetFromMatrix
#' @importFrom stats as.formula
#'
#' @export

Run.DESeq2 <- function(expr.data, sample.data, formula, test, fitType, sfType,
                       betaPrior, reduced, minReplicatesForReplace, modelMatrixType, useT, minmu) {
  data <- expr.data[, sample.data$samples]
  dds <- DESeqDataSetFromMatrix(data, colData = sample.data, design = as.formula(formula))
  dds <- dds[rowSums(counts(dds)) >= 1, ]

  if (test == "LRT") {
    dds <- DESeq(object = dds, test = test, fitType = fitType, sfType = sfType, betaPrior = as.logical(betaPrior),
                 reduced = reduced, minReplicatesForReplace = minReplicatesForReplace, modelMatrixType = modelMatrixType,
                 useT = as.logical(useT), minmu = minmu)
  }else {
    dds <- DESeq(object = dds, test = test, fitType = fitType, sfType = sfType, betaPrior = as.logical(betaPrior),
                 minReplicatesForReplace = minReplicatesForReplace, modelMatrixType = modelMatrixType,
                 useT = as.logical(useT), minmu = minmu)
  }

  if (!dir.exists('./Cache')) {
    dir.create('./Cache')
  }
  saveRDS(dds, "Cache/DESeq_object.rds")

  return(dds)
}

#' Remove batch effects from data
#'
#' @param expr.data expression data matrix
#' @param designTable experiment design table
#' @param key_words key words for batch factors
#' @param design unknown, conditions ?
#' @param method methods used to correct batch effects
#' @importFrom sva ComBat
#' @importFrom limma removeBatchEffect
#' @importFrom stats model.matrix
#' @export

remove.Batch <- function(expr.data, designTable, key_words = "batch", design = "condition", method = 'ComBat') {
  if (!key_words %in% colnames(designTable)) {
    stop("Can not find '", key_words ,"' in the sampleTable colnames, please check your input ...", call. = FALSE)
  }
  if (method == "ComBat") {
    mod = model.matrix(~as.factor(designTable[, design]), data = designTable)
    corrected.expr <- ComBat(dat = expr.data, batch = designTable[, key_words], mod = mod)
  }else {
    corrected.expr <- removeBatchEffect(x = expr.data, batch = designTable[, key_words], design = designTable)
  }
  return(corrected.expr)
}

#' Transform normalized expression matrix by vst or rlog
#'
#' @param object dds object
#' @param blind logical, whether to blind the transformation to the experimental design.
#' @param fitType either "parametric", "local", or "mean" for the type of fitting of dispersions to the mean intensity.
#' @param nsub the number of genes to subset to (default 1000)
#' @param trans.method method for transforming data, 'rlog' or 'vst'
#' @param batch.method method for remove batch effects, 'ComBat', 'removeBatchEffect' or NULL
#' @param key_words a column name will treated as batch effector
#' @importFrom DESeq2 rlog vst
#' @export
#'
transform_value <- function(object, blind, fitType, nsub, trans.method, batch.method = 'NULL', key_words = NULL) {
  if (trans.method == "rlog") {
    trans_data <- rlog(object, blind = as.logical(blind), fitType = fitType)
  }else {
    trans_data <- vst(object, blind = as.logical(blind), nsub = nsub, fitType = fitType)
  }

  if (batch.method != 'NULL') {
    cor.expr <- remove.Batch(expr.data = assay(trans_data), designTable = subset(trans_data@colData, select = -sizeFactor),
                                 key_words = key_words, design = "condition", method = batch.method)

    assay(trans_data) <- cor.expr
  }

  if (!dir.exists('./Cache')) {
    dir.create('./Cache')
  }

  if (trans.method == "rlog") {
    saveRDS(trans_data, "Cache/Deseq_rlog.rds")
  }else {
    saveRDS(trans_data, "Cache/Deseq_vst.rds")
  }

  return(trans_data)
}



#' Get A List Of Differentially Expressed Genes
#'
#' @param dds object of produced by DESeq
#' @param ctrl Control group
#' @param treat TreatMent groups
#' @param p.adjust Adjusted P-values
#' @param abs.lfc Log2 FoldChange Abs values
#' @param save save DEG List to Local Directory
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



#' Subset The Experiment design Table while keep the order
#'
#' @param dds object of produced by DESeq
#' @param condition conditions to keep
#'
#' @return Data.Frame
#' @export
#'

subset_Tab <- function(dds, condition) {
  sampleTable <- as.data.frame(colData(dds))
  rownames(sampleTable) <- sampleTable$samples
  idx <- lapply(condition, function(x){
    sampleTable[sampleTable$condition == x, "samples"]
  }) %>% unlist
  idx <- idx[idx %in% rownames(sampleTable)]
  sampleTable <- sampleTable[idx, ]
  return(sampleTable)
}
