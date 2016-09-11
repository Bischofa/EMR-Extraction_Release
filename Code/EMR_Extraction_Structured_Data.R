# Script to extract structured data
# 1.Extraction of demographics data
# 2.Extraction of labs data
# 3.Extraction of medications data
#
# Copyright Vincent Damotte & Antoine Lizee 09/2015 @ UCSF
# antoine.lizee@gmail.com

library(tidyr)
library(plyr)


# Define General variables ------------------------------------------------
#In EMR dataset, to keep only patients that have been classified into one of the 4 classification groups
definedMRN <- Patients_Classification$PAT_MRN_ID[!Patients_Classification$UCSFClassification == "Unclassified"]


# Extraction of demographics data -----------------------------------------
cat("\n### 04.01.01.Extraction of demographics data\n")

#To keep only patients that have been classified into one of the 4 classification group
demos_extracted <- TABLES$demos[TABLES$demos$PAT_MRN_ID %in% definedMRN, ]

#Age computation
Reference <- as.Date("2015-06-09", format="%Y-%m-%d") #Date of CHR Approval
DOB <- demos_extracted$DOB
age <- ceiling(as.numeric(Reference-DOB)/365.25)

demos_extracted <- data.frame(demos_extracted[c(1,2)], Age = age, demos_extracted[3:ncol(demos_extracted)])

cat("Extraction of demographics data done\n")


# Extraction of labs data -------------------------------------------------
cat("\n### 04.01.02.Extraction of labs data\n")

#To keep only patients that have been classified into one of the 4 classification group
labs_extracted <- TABLES$labs[TABLES$labs$PAT_MRN_ID %in% definedMRN, ]

Component_Names=c("VITAMIND, 25-HYDROXY", "VITAMIN B 12",
                  "SED RATE", "CRP HIGH SENSITIVITY", "PLATELETCOUNT",
                  "LYMPHOCYTEABSCNT", "NEUTROPHILABSOLUTECOUNT", "WBCCOUNT",
                  "AST", "ALT",
                  "VARICELLA ZOSTER AB (EXTERNAL LAB)", "VARICELLA ZOSTER AB (EXTERNAL LAB - 1)", 
                  "VARICELLA-ZOSTERANTIBODY, IGG")

labs_extracted <- labs_extracted[labs_extracted$ComponentName %in% Component_Names, ]
stopifnot(!anyDuplicated(labs_extracted[, c("PAT_MRN_ID", "ResultDate", "ComponentName")]))
labs_extracted <- labs_extracted[, c("PAT_MRN_ID", "ResultDate", "ComponentName", "Value", "Unit")]

# Varicella Zoster Antibody
labs_extracted$ComponentName <- sub("VARICELLA ZOSTER AB (EXTERNAL LAB)", "VARICELLA-ZOSTERANTIBODY", labs_extracted$ComponentName, fixed = T)
labs_extracted$ComponentName <- sub("VARICELLA ZOSTER AB (EXTERNAL LAB - 1)", "VARICELLA-ZOSTERANTIBODY", labs_extracted$ComponentName, fixed = T)
labs_extracted$ComponentName <- sub("VARICELLA-ZOSTERANTIBODY, IGG", "VARICELLA-ZOSTERANTIBODY", labs_extracted$ComponentName, fixed = T)

labs_extracted$Value <- sub("POS", "Positive", labs_extracted$Value, fixed = T)
labs_extracted$Value <- sub("NEG", "Negative", labs_extracted$Value, fixed = T)

cat("Extraction of labs data done\n")


# Extraction of medications data ------------------------------------------
cat("\n### 04.01.03.Extraction of medications data\n")

#To keep only patients that have been classified into one of the 4 classification group
meds_extracted <- TABLES$meds[ TABLES$meds$PAT_MRN_ID %in% definedMRN, ]

Medic_Names_Categories = list(PEGINTERFERON_BETA_1A = c("PEGINTERFERON BETA-1A", "PLEGRIDY"),
                            INTERFERON_BETA_1A = c("INTERFERON BETA-1A", "AVONEX", "REBIF"),
                            INTERFERON_BETA_1B = c("INTERFERON BETA-1B", "BETASERON", "EXTAVIA"),
                            GLATIRAMER_ACETATE = c("GLATIRAMER ACETATE", "GLATIRAMER","COPAXONE"),
                            NATALIZUMAB = c("NATALIZUMAB", "TYSABRI"),
                            TERIFLUNOMIDE = c("TERIFLUNOMIDE", "AUBAGIO"),
                            FINGOLIMOD = c("FINGOLIMOD", "GILENYA"), 
                            DIMETHYL_FUMARATE = c("DIMETHYL FUMARATE", "TECFIDERA"),
                            MITOXANTRONE = c("MITOXANTRONE", "NOVANTRONE"),
                            ALEMTUZUMAB = c("ALEMTUZUMAB", "LEMTRADA"),
                            DALFAMPRIDINE = c("DALFAMPRIDINE","AMPYRA") )

meds_extracted$Med_Name <- rep(NA, nrow(meds_extracted))
for (i in 1:length(Medic_Names_Categories)){
  matchIndices <- grepl(paste(Medic_Names_Categories[[i]], collapse="|"),
                        meds_extracted$Med_Desc, ignore.case = T)
  meds_extracted$Med_Name[matchIndices] <- names(Medic_Names_Categories)[i]
}

cat("Extracted the Meds in the following quantities:")
medN <- table(meds_extracted$Med_Name)
print(cbind(medN[order(medN, decreasing = T)]))

cat(sprintf("Removing %d Meds that are not MS related.\n", sum(!(isMSDrug <- !is.na(meds_extracted$Med_Name)))))
meds_extracted <- meds_extracted[isMSDrug, c("PAT_MRN_ID", "Med_Order_Start_Date", "Med_Order_End_Date", "Med_Desc", "Med_Name")]

cat("Extraction of medications data done\n")

cat("\n Extraction of structured data done\n\n")
