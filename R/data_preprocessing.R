#' Get support species names
#'
#' @export
#'
get_supported_species <- function(datasets = "gprofiler") {
  ids <- readRDS(system.file("extdata", "species.rds", package = "QRseq"))
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

#' Searching GEO Datasets by keywords
#'
#' @param db character, name of the database to search for.
#' @param organism character, organism to search for.
#' @param title character, keywords cantained in title
#' @param title_lg character, the logical connection between next search fields.
#' @param description character, keywords cantained in description
#' @param description_lg character, the logical connection between next search fields.
#' @param datasetType character, dataset type
#' @param datasetType_lg character, the logical connection between next search fields.
#' @param use_history logical. If TRUE return a web_history object for use in later calls to the NCBI
#' @importFrom dplyr bind_rows
#' @importFrom rentrez entrez_search entrez_fetch
#'
#' @return data.frame
#' @export
#'

Search.GEO <- function(db = "gds", organism = NULL, title = NULL, title_lg = NULL, description = NULL,  description_lg = NULL,
                       datasetType = "Expression profiling by high throughput sequencing",  datasetType_lg = "AND", use_history = T) {

  if (length(c(title, description, organism, datasetType)) == 1) {
    search_term <- paste0(datasetType, "[DataSet Type]")
  }else {
    if (!is.null(organism)) {
      search_term <- paste("(", organism, "[Organism])", datasetType_lg, datasetType, "[DataSet Type]", sep = " ") # search with organism and dataset type
    }
    if (!is.null(title)) {
      search_term <- paste("(", search_term, ")", title_lg, title, "[Title]") # add title to search term if it isn't none
    }
    if (!is.null(description)) {
      search_term <- paste("(", search_term, ")", description_lg, description, "[Description]") # add description to search term if it isn't none
    }
  }

  search_results <- entrez_search(db = db, term = search_term, use_history = use_history) # search
  xml_results <- entrez_fetch(db = db, web_history = search_results$web_history, rettype = "xml") # fetch results

  if (!grepl("<ERROR>Empty result", x = xml_results)) {
    xml_clean <- stringr::str_split(xml_results, "\n[0-9]+. ")[[1]][-1] # split results by studies

    xml_list <- lapply(xml_clean, function(x){
      list_1 <- stringr::str_split(x, "\n")[[1]]
      list_2 <- lapply(list_1, function(x){
        gsub("[ \t]+", " ", x)
      })
    }) # split reults by terms

    xml_split_list <- lapply(xml_list, function(qlist){
      if (grepl(pattern = "(Submitter supplied)", x = qlist[[2]])) {
        # create a empty data.frame
        df_empty <- data.frame(
          Title = character(),
          Description = character(),
          Organism = character(),
          Type = character(),
          Platform = character(),
          FTP_download = character(),
          SRA_Run_Selector = character(),
          Series_Accession = character(),
          row.names = 1,
          stringsAsFactors = F
        )

        for (i in 1:length(qlist)) {
          if (i==1) {
            df_empty[1, "Title"] <- qlist[[i]]
          }
          if (grepl(pattern = "(Submitter supplied)", x = qlist[[i]])) {
            df_empty[1, "Description"] <- gsub(x = qlist[[i]], pattern = "\\(Submitter supplied\\)", replacement = "")
          }
          if (grepl(pattern = "Organism:", x = qlist[[i]])) {
            df_empty[1, "Organism"] <- gsub(x = qlist[[i]], pattern = "Organism:", replacement = "")
          }
          if (grepl(pattern = "Type:", x = qlist[[i]])) {
            df_empty[1, "Type"] <- gsub(x = qlist[[i]], pattern = "Type:", replacement = "")
          }
          if (grepl(pattern = "Platform: ", x = qlist[[i]])) {
            df_empty[1, "Platform"] <- gsub(x = qlist[[i]], pattern = "Platform:", replacement = "")
          }
          if (grepl(pattern = "FTP download:", x = qlist[[i]])) {
            df_empty[1, "FTP_download"] <- gsub(x = qlist[[i]], pattern = "FTP download:", replacement = "")
          }
          if (grepl(pattern = "SRA Run Selector:", x = qlist[[i]])) {
            df_empty[1, "SRA_Run_Selector"] <- gsub(x = qlist[[i]], pattern = "SRA Run Selector:", replacement = "")
          }
          if (grepl(pattern = "Series Accession:", x = qlist[[i]])) {
            Accession <- gsub(x = qlist[[i]], pattern = "Series Accession:", replacement = "")
            df_empty[1, "Series_Accession"] <- gsub(x = Accession, pattern = "ID:.*", replacement = "")
          }
        } # use for loop to fill values into data.frame
        return(df_empty)
      }else {
        return(NULL)
      }
    }) # split and create a df for terms and return a list

    result_df <- bind_rows(xml_split_list)
    result_df <- result_df[, c("Organism", "Series_Accession", "Title", "Description",  "Type", "Platform", "FTP_download","SRA_Run_Selector")]

    return(result_df)
  }else {
    error_df <- data.frame(Empty_result = "<ERROR>Empty result - nothing to do</ERROR>", row.names = 1, stringsAsFactors = F)
    return(error_df)
  }
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

