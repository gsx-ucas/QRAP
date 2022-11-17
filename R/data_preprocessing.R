#' Get support species names
#'
#' @export
#'
get_supported_species <- function(datasets = "gprofiler") {
  ids <- readRDS(system.file("extdata", "species.rds", package = "QRAP"))
  ids$display_name <- paste0(ids$scientific_name, " (", ids$display_name, ")")
  ids <- ids[, -2]
  return(ids)
}

#' Function to import files by automatically identify delimiters
#'
#' @param file_path the name of the file which the data are to be read from.
#' @param header a logical value indicating whether the file contains the names of the variables as its first line.
#' @param row_names a vector of row names.
#' @param stringsAsFactors logical: should character vectors be converted to factors?
#' @importFrom utils unzip read.table read.csv
#'
#' @return data.frame
#' @export
#'
read_files <- function(file_path = NULL, header = TRUE, row_names = 1, stringsAsFactors = F) {
  # read the second line to identify delimiters
  if (grepl(x = file_path, pattern = ".zip")) {
    Lines_2 <- readLines(unzip(file_path), n = 2)[2]
  } else {
    Lines_2 <- readLines(file_path, n = 2)[2]
  }
  # # identify delimiters
  if (grepl(x = Lines_2, pattern = "\t") == T) {
    delimiters <- "\t"
  } else if (grepl(x = Lines_2, pattern = ",") == T) {
    delimiters <- ","
  } else if (grepl(x = Lines_2, pattern = " ") == T) {
    delimiters <- " "
  } else if (grepl(x = Lines_2, pattern = ";") == T) {
    delimiters <- ";"
  } else if (grepl(x = Lines_2, pattern = "|") == T) {
    delimiters <- "|"
  } else if (grepl(x = Lines_2, pattern = ":") == T) {
    delimiters <- ":"
  }
  # # use read.csv function to import files
  if (grepl(x = file_path, pattern = ".zip")) {
    data <- read.csv(file = unzip(file_path), header = header, row.names = row_names, sep = delimiters, check.names = F, stringsAsFactors = stringsAsFactors)
  } else {
    data <- read.csv(file = file_path, header = header, row.names = row_names, sep = delimiters, check.names = F, stringsAsFactors = stringsAsFactors)
  }
  return(data)
}

#' Load in data sets and creat dataframe of Reads Number
#'
#' @param path the name of the file which the data are to be read from.
#' @param header a logical value indicating whether the file contains the names of the variables as its first line.
#' @param row_names a logical value indicating whether the file contains the names of the variables as its first column.
#'
#' @return data.frame
#' @export
#'

upload.data <- function(path, header, row_names = TRUE) {
  if (isTRUE(row_names)) {
    df <- read_files(file_path = path, header = header, row_names = 1)
  } else {
    df <- read_files(file_path = path, header = header, row_names = NULL)
  }
  df <- df[order(colnames(df))]
  return(df)
}

#' Download GEO Datasets or supplementary files
#'
#' @param geoID GEO Accession Number
#' @param out_dir where the files will download to
#' @importFrom GEOquery getGEO getGEOSuppFiles
#' @export

download.GEO <- function(geoID, out_dir = paste0(getwd(), "/GEO_Download")) {

  geoID <- toupper(geoID)

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = T)
  }

  gds_tb <- readRDS(system.file("extdata", "gds_datasets.rds", package = "QRAP"))
  if (geoID %in% gds_tb$Series) {
    incProgress(0.3, detail = "Downloading series_matrix ...")
    GEO.File <- getGEO(geoID, destdir = tempdir(), getGPL = FALSE, parseCharacteristics = FALSE, AnnotGPL = FALSE)
  }else {
    incProgress(0.3, detail = "Downloading GEOSuppFiles ...")
    GEO.File <- getGEOSuppFiles(geoID, baseDir = out_dir)
  }

  if (is.data.frame(GEO.File)) {
    tar_files <- grep(x = rownames(GEO.File), pattern = "_raw.tar", ignore.case = T, value = T)
    if (length(tar_files) != 0) {
      incProgress(0.3, detail = "untar downloaded files ...")
      untar(tarfile = tar_files, exdir = paste0(out_dir, geoID))
      file.remove(tar_files)
    }
    GEO.File <- dir(paste0(out_dir, geoID))
  }
  return(GEO.File)
}

#' Read in and preprocessing download files
#'
#' @param fetched_res object of geo.Download
#' @param geoID GEO Accession Number
#' @param geo_files a vector of input file names
#' @param genes_column the column number will used as gene names
#' @param reads_column the column number will used as reads counts of samples
#' @param header whethere use first row as header
#' @param row_names whethere use first column as rownames
#' @importFrom stringr str_remove
#' @importFrom plyr join_all
#'
#' @export
#'
#' @return data.frame

preview.GEO <- function(fetched_res, geoID, geo_files, genes_column, reads_column, header, row_names) {
  if (is.list(fetched_res)==TRUE) {
    matrix <- fetched_res[[geo_files]]@assayData$exprs
  }else {
    file_dir <- paste0(getwd(), "/GEO_Download/", geoID)
    if (geo_files %>% length > 1) {
      matrix <- lapply(geo_files, function(x){
        files <- paste0("./GEO_Download/", geoID, "/", x)
        saname <- str_remove(x, "\\..*")
        df <- read_files(file_path = files, row_names = NULL, header = header, stringsAsFactors = F)
        df <- df[ ,c(genes_column, reads_column)]
        colnames(df) <- c("genes", saname)
        return(df)
      }) %>% join_all(by = "genes", type = "inner", match = "first")

      matrix <- matrix[!duplicated(matrix[, 1]), ]
      rownames(matrix) <- matrix[, 1]
      matrix <- matrix[, -1]
    }else {
      matrix <- read_files(paste(file_dir, geo_files, sep = "/"), header = header, row_names = NULL)
      if (isTRUE(row_names)) {
        matrix <- matrix[!duplicated(matrix[, 1]), ]
        rownames(matrix) <- matrix[, 1]
        matrix <- matrix[, -1]
      }
    }
  }
  return(matrix)
}

