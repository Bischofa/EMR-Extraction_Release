# Script to perform QC and cleaning on tables


# Simple Cleaning of all tables -------------------------------------------
emr_table_names <- setdiff(table_names, table_non_emr)

TABLES[emr_table_names] <- lapply(emr_table_names, function(df_name) {
  
  df <- TABLES[[df_name]]
  cat(sprintf("\n## 02.00.. Cleaning of table: %s\n", df_name))
  
  cat("# Colnames cleaning\n")
  colnames(df) <- gsub(" ","_", colnames(df))
  cat(" Colnames cleaned\n")
  
  cat("# Ordering rows per PAT_MRN_ID\n")
  df <- df[order(df$PAT_MRN_ID),]
  cat(" Rows ordered\n")
  
  return(removeDuplicates(df, df_name))
  
})

# Cleaning of demographics data -------------------------------------------

cat("\n## 02.01.Cleaning of demographics data\n")

cat("# Colnames further cleaning\n")
colnames(TABLES$demos)[2] <- "DOB"
colnames(TABLES$demos)[4] <- "Ethnicity"
cat(" Colnames cleaned\n")

demos_QC <- TABLES$demos

cat("# Removing duplicated PAT_MRN_IDs in demos_QC\n")
duplicated_PAT_MRN_ID = demos_QC[duplicated(demos_QC$PAT_MRN_ID),"PAT_MRN_ID"]

if (length(duplicated_PAT_MRN_ID) > 0) {
  pattern <- "Unknown|Unknown/Declined|NULL"
  n_matches_Ethnicity <- sapply(gregexpr(pattern, demos_QC$Ethnicity, ignore.case = T), function(x) if(is.na(x) || x[1] == -1) 0 else length(x))
  n_matches_Race <- sapply(gregexpr(pattern, demos_QC$Race, ignore.case = T), function(x) if(is.na(x) || x[1] == -1) 0 else length(x))
  demos_QC <- demos_QC[order(demos_QC$PAT_MRN_ID, n_matches_Race, n_matches_Ethnicity),]
  demos_QC <- demos_QC[-which(duplicated(demos_QC$PAT_MRN_ID)),]
  cat(paste("INPUT_ARGS_WARNING:", "In demos_QC,",length(duplicated_PAT_MRN_ID), "duplicated PAT_MRN_ID has been removed.\n"))
} else {cat("No duplicated rows in demos_QC \n")}
stopifnot(nrow(demos_QC[duplicated(demos_QC$PAT_MRN_ID),"PAT_MRN_ID"])==0)

cat("# Symplifying Ethnicity and Race labels in demos_QC\n")
demos_QC$Ethnicity <- ifelse(demos_QC$Ethnicity == "NULL" | demos_QC$Ethnicity == "Unknown/Declined", "Unknown", demos_QC$Ethnicity)
demos_QC$Race <- ifelse( demos_QC$Race=="NULL" | demos_QC$Race=="Unknown/Declined", "Unknown", demos_QC$Race)
table(demos_QC$Ethnicity, demos_QC$Race)

TABLES$demos <- demos_QC
rm(demos_QC)
cat("# Cleaning of demographics data done\n")


# Cleaning of labs data ---------------------------------------------------

cat("\n## 02.02.Cleaning of labs data\n")
labs_QC <- TABLES$labs

cat("# Keeping only needed labs\n")
Component_Names=c("VITAMIND, 25-HYDROXY", "VITAMIN B 12",
                  "SED RATE", "CRP HIGH SENSITIVITY", "PLATELETCOUNT",
                  "LYMPHOCYTEABSCNT", "NEUTROPHILABSOLUTECOUNT", "WBCCOUNT",
                  "AST", "ALT",
                  "VARICELLA ZOSTER AB (EXTERNAL LAB)", "VARICELLA ZOSTER AB (EXTERNAL LAB - 1)", 
                  "VARICELLA-ZOSTERANTIBODY, IGG")

labs_QC <- labs_QC[labs_QC$ComponentName %in% Component_Names, ]
cat(" Removed", nrow(TABLES$labs) - nrow(labs_QC), "labs.\n")

cat("# Removing labs rows with no value or no result date and with result status different from 'Final'\n")
isBadLab <- is.na(labs_QC$Value) | labs_QC$Value == "NULL" | labs_QC$Value == "Insufficient cells present in specimen for reliable result. Suggest repeat specimen if clinically indicated." | is.na(labs_QC$ResultDate) | labs_QC$ResultStatus != "Final"
labs_QC = labs_QC[!isBadLab, ]
cat(" Removed", sum(isBadLab), "non-usable labs.\n")

cat("# Keeping the last OrderID when several results for the same labs at the same date\n")
isDuplicated <- duplicated(labs_QC[, c("PAT_MRN_ID", "ResultDate", "ComponentName")]) | 
  duplicated(labs_QC[, c("PAT_MRN_ID", "ResultDate", "ComponentName")], fromLast = T)
duplicatedLabs <- labs_QC[isDuplicated,]
if ( (nDups <- sum(isDuplicated)) > 0 ) {
  retainedDuplicates <- ddply(duplicatedLabs, ~ PAT_MRN_ID + ResultDate + ComponentName, function(dfi) {
    dfi[order(dfi$OrderID, decreasing = T)[1],]
  })
  labs_QC <- rbind(labs_QC[!isDuplicated,], retainedDuplicates)
  cat(paste("INPUT_ARGS_WARNING:", "In labs_QC,",
                nDups - nrow(retainedDuplicates),
                "duplicated labs with different results at the same date have been removed.\n"))
}

TABLES$labs <- labs_QC
rm(labs_QC)
cat("# Cleaning of labs data done\n")


# Cleaning of medications data --------------------------------------------

cat("\n## 02.03.Cleaning of medications data\n")
meds_QC <- TABLES$meds

cat("# Looking at medications with no associated posology\n")
missingPos <- meds_QC$HV_DOSE_UNIT_C=="NULL"
cat("NOT Removing ", sum(missingPos), " (over ", length(missingPos), ") medications without posology.\n", sep = "")


cat("# Lokking at medications with no Start Med Order\n")
missingStartDate <- is.na(meds_QC$Med_Order_Start_Date)
cat("NOT Removing ", sum(missingStartDate), " (over ", length(missingStartDate), ") medications without Start Date.\n", sep = "")


TABLES$meds <- meds_QC
rm(meds_QC)
cat("# Cleaning done\n")


# Cleaning of notes table -------------------------------------------------
cat("\n## 02.04.Cleaning of notes data\n")
notes_QC <- TABLES$notes

cat("# Conversion from POSIXct date format to standard date format of CONTACT_DATE column\n")
notes_QC$CONTACT_DATE <- as.Date(notes_QC$CONTACT_DATE)
cat("Done\n")

TABLES$notes <- notes_QC
rm(notes_QC)
cat("# Cleaning done\n")


cat("# Cleaning of all tables done\n\n")