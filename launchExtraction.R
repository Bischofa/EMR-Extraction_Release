# launchExtraction.R Info  ------------------------------------------------
# Script to launch the extraction of the de-identified medical records present in the folder Files_Stored
# The script operates per subfolder.
# The script is meant to be launched on the top level.
#
# Antoine Lizee & Vincent Damotte @ UCSF 09/14
# antoine.lizee@ucsf.edu


# Setup ----------------------------------------------------------
rm(list=ls())

library("ggplot2")
library("plyr")
library("reshape2")
library("readr")


# Set parameters to launch ------------------------------------------------

dataset <- "Dataset_Name" # Same name as the name of the dataset folder in Files_Stored and Files_Output folders
cat("### Running extraction algorithm for", dataset, "dataset ###\n")

file_names <- c(demos = "Demographics.csv",
                dx = "Dx.csv",
                labs = "Labs.csv",  
                meds = "Meds.csv",
                notes = "Notes_text.csv"
                ) 


# File Paths Setup --------------------------------------------------------

items <- dir()
stopifnot(c("Code","Files_Output","Files_Stored","launchExtraction.R") %in% items) # Check that the path is right
outputPath <- file.path("Files_Output", dataset)
inputPath <- file.path("Files_Stored", dataset)


# Helpers -----------------------------------------------------------------

readCSV <- function(name, fileName = file_names[name], na.rm = F, sep = NULL, ...) {
  cat("## Reading", fileName, "...\n")
  if (is.null(sep)) {
    res <- suppressWarnings(read_csv(file.path(inputPath, fileName), col_types = cols(PAT_MRN_ID = col_integer())))
    pbs <- problems(res)
    if (nrow(pbs)) {
      cat(sprintf("# %d problems looking like:\n", nrow(pbs)))
      print(head(pbs))
      cat(sprintf("# %d unique problems:\n", nrow(pbsU <- unique(pbs[-1]))))
      print(head(pbsU), 20)
    }
  } else {
    res <- read.csv(file.path(inputPath, fileName), sep = sep, stringsAsFactors = FALSE)
  }
  resNoNA <- na.omit(res)
  na_n <- nrow(res) - nrow(resNoNA)
  if (na_n == 0) {
    return(res)
  }
  if (na.rm) {
    cat(sprintf("Removed %d lines (%.2f%%) with NA.\n-------\n", na_n, na_n / nrow(res) * 100))
    return(resNoNA)
  } else {
    cat(sprintf("DID NOT Removed %d lines (%.2f%%) with NA.\n-------\n", na_n, na_n / nrow(res) * 100))
    return(res)
  }
}

writeCSV <- function(df, fileName) {
  write_csv(x = df, path = file.path(outputPath, paste0(fileName, ".csv")))
}



# Read in Data ------------------------------------------------------------

cat("\n### 01.Read in EMR files\n\n")
table_names <- names(file_names)
table_non_emr <- c("ethnicity_ref")
table_with_colons <- c("ethnicity_ref")

TABLES <- sapply(table_names, readCSV)

cat("\n\n### All tables loaded\n\n")


# Run files through code --------------------------------------------------
source(file.path("Code","EMR_Extraction_Functions.R"))

cat("\n### 02.Data cleaning\n\n")
source(file.path("Code","EMR_Cleaning.R"))

cat("\n### 03.MS patients identification and classification\n\n")
source(file.path("Code","EMR_Classification.R"))

cat("\n### 04.Extraction\n\n")

cat("\n### 04.01.Structured data extraction\n")
source(file.path("Code","EMR_Extraction_Structured_Data.R"))

cat("\n### 04.02.Untructured data extraction\n")
source(file.path("Code","EMR_Extraction.R"))

cat("\n### 05.Statistics computation\n")
source(file.path("Code","EMR_Extraction_Stats.R"))

# Export Files in CSV -----------------------------------------------------

cat("\n### 05.Exporting extracted tables\n\n")
writeCSV(ALL_Table, "Patients_Classification_all_info")
writeCSV(Patients_Classification, "Patients_Classification")
writeCSV(demos_extracted, "Demographics_Extracted")
writeCSV(labs_extracted, "Labs_Extracted")
writeCSV(meds_extracted, "Meds_Extracted")
writeCSV(EDSS.matches, "EDSS_01_Extracted")
writeCSV(EDSS.final, "EDSS_02_Denormalized")
writeCSV(walk.matches, "25ft_01_Extracted")
writeCSV(walk.final, "25ft_02_Denormalized")
writeCSV(subtype.matches, "Subtype_01_Extracted")
writeCSV(subtype.final, "Subtype_02_Denormalized")
writeCSV(onset.matches, "Onset_01_Extracted")
writeCSV(onset.final, "Onset_02_Denormalized")

cat(" Done\n\n")

