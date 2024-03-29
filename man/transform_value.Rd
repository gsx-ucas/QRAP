% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runing_functions.R
\name{transform_value}
\alias{transform_value}
\title{Transform normalized expression matrix by vst or rlog}
\usage{
transform_value(
  object,
  blind,
  fitType,
  nsub,
  trans.method,
  batch.method = "NULL",
  batch = NULL,
  batch2 = NULL
)
}
\arguments{
\item{object}{dds object}

\item{blind}{logical, whether to blind the transformation to the experimental design.}

\item{fitType}{either "parametric", "local", or "mean" for the type of fitting of dispersions to the mean intensity.}

\item{nsub}{the number of genes to subset to (default 1000)}

\item{trans.method}{method for transforming data, 'rlog' or 'vst'}

\item{batch.method}{method for remove batch effects, 'ComBat', 'removeBatchEffect' or NULL}

\item{batch}{a column name will treated as batch effector}
}
\description{
Transform normalized expression matrix by vst or rlog
}
