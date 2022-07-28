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
                 reduced = as.formula(reduced), minReplicatesForReplace = minReplicatesForReplace, modelMatrixType = modelMatrixType,
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
#' @param batch key words for batch factors
#' @param design unknown, conditions ?
#' @param method methods used to correct batch effects
#' @importFrom sva ComBat
#' @importFrom limma removeBatchEffect
#' @importFrom stats model.matrix
#' @export

remove.Batch <- function(expr.data, designTable, batch = "batch", batch2 = NULL, design = "condition", method = 'ComBat') {
  if (!batch %in% colnames(designTable)) {
    stop("Can not find '", batch ,"' in the sampleTable colnames, please check your input ...", call. = FALSE)
  }
  mod = model.matrix(~as.factor(designTable[, design]), data = designTable)
  if (method == "ComBat") {
    corrected.expr <- ComBat(dat = expr.data, batch = designTable[, batch], mod = mod)
  }else {
    if (is.null(batch2)) {
      corrected.expr <- removeBatchEffect(x = expr.data, batch = as.factor(designTable[, batch]), design = mod)
    }else {
      corrected.expr <- removeBatchEffect(x = expr.data, batch = as.factor(designTable[, batch]), batch2 = as.factor(designTable[, batch2]), design = mod)
    }
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
#' @param batch a column name will treated as batch effector
#' @importFrom DESeq2 rlog vst
#' @export
#'
transform_value <- function(object, blind, fitType, nsub, trans.method, batch.method = 'NULL', batch = NULL, batch2 = NULL) {
  if (trans.method == "rlog") {
    trans_data <- rlog(object, blind = as.logical(blind), fitType = fitType)
  }else {
    trans_data <- vst(object, blind = as.logical(blind), nsub = nsub, fitType = fitType)
  }

  if (batch.method != 'NULL') {
    cor.expr <- remove.Batch(expr.data = SummarizedExperiment::assay(trans_data), designTable = subset(trans_data@colData, select = -sizeFactor),
                                 batch = batch, batch2 = batch2, design = "condition", method = batch.method)

    SummarizedExperiment::assay(trans_data) <- cor.expr
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

