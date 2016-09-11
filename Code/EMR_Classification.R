# EMR_Classification
# This script retrieves MS patients from an EMR cohort
#
# Copyright Vincent Damotte & Antoine Lizee @ UCSF, 04/2015


# Import data needed for classification -----------------------------------

#For the classification algorithm, we need:
#ICD-9 codes attributed to Diagnotics,
#Medications
#Clinical Notes

classificationIds <- data.frame(PAT_MRN_ID = unique(unlist(lapply(TABLES[c("dx", "meds", "notes")], "[[", "PAT_MRN_ID"))))
putInGlobalEnv(TABLES[c("dx", "meds", "notes")])


# Searching for MS or related symptoms in diagnostics ICD-9 codes ------------------------
#340: MS
#323.9: Unspecified causes of encephalitis, myelitis, and encephalomyelitis
#341.2: Acute (transverse) myelitis
#341.21: Acute (transverse) myelitis in conditions classified elsewhere
#341.8: Other demyelinating diseases of central nervous system
#341.9: Demyelinating disease of central nervous system, unspecified
#377.3: Optic neuritis

ICD9_Codes_Categories <- list( WellDefined = "340",
                               ProbableA = c("341.2","341.21","341.9", "323.9"),
                               ProbableB = c("341.2","341.21","341.9", "341.8","377.3"))
ICD9_Codes = unique(unlist(ICD9_Codes_Categories))

dx$PAT_MRN_ID <- factor(dx$PAT_MRN_ID)
# Code occurences per code and patient:
Table_ICD9_MS <- table(x = dx[dx$ICD9_Code %in% ICD9_Codes, 1:2])

# let's count the number of *different* codes present for each patient for each category:
Table_ICD9_MS_cat <- sapply(ICD9_Codes_Categories, function(codes) {
  apply(Table_ICD9_MS[, colnames(Table_ICD9_MS) %in% codes, drop = FALSE], 1, function(codeRow) sum(codeRow > 0))
})
# have a look at the numbers, per category:
apply(Table_ICD9_MS_cat, 2, table)
Table_ICD9_MS_cat <- as.data.frame(Table_ICD9_MS_cat)
Table_ICD9_MS_cat <- data.frame(PAT_MRN_ID = as.integer(rownames(Table_ICD9_MS_cat)),
                                Table_ICD9_MS_cat,
                                stringsAsFactors = F)


# Searching for ICD9 codes excluding MS -----------------------------------
#List of ICD9 codes to search for
ICD9_Codes_MSexcl=c("330.0",  #LEUKODYSTROPHY
                    "330.1",  #CEREBRAL LIPIDOSES
                    "330.2",  #CEREBRAL DEGENERATION IN GENERALIZED LIPIDOSES
                    "330.3",  #CEREBRAL DEGENERATION OF CHILDHOOD IN OTHER DISEASES CLASSIFIED ELSEWHERE
                    "330.8",  #OTHER SPECIFIED CEREBRAL DEGENERATIONS IN CHILDHOOD
                    "330.9",  #UNSPECIFIED CEREBRAL DEGENERATION IN CHILDHOOD
                    "341.0",  #NEUROMYELITIS OPTICA
                    "341.1",  #SCHILDER'S DISEASE
                    "290.40", #VASCULAR DEMENTIA, UNCOMPLICATED
                    "290.41", #VASCULAR DEMENTIA, WITH DELIRIUM
                    "290.42", #VASCULAR DEMENTIA, WITH DELUSIONS
                    "290.43", #VASCULAR DEMENTIA, WITH DEPRESSED MOOD
                    "135",    #SARCOIDOSIS
                    "323.81", #OTHER CAUSES OF ENCEPHALITIS AND ENCEPHALOMYELITIS
                    "335.20", #AMYOTROPHIC LATERAL SCLEROSIS
                    "335.21", #PROGRESSIVE MUSCULAR ATROPHY
                    "335.22", #PROGRESSIVE BULBAR PALSY
                    "335.23", #PSEUDOBULBAR PALSY
                    "335.24", #PRIMARY LATERAL SCLEROSIS
                    "335.29",  #OTHER MOTOR NEURON DISEASES
                    "277.87" #MELAS
                    # Should exclude too much, but to be checked:
                    )

ICD9_Codes_MSexclGeneral = c("794.00", #UNSPECIFIED ABNORMAL FUNCTION STUDY OF BRAIN AND CENTRAL NERVOUS SYSTEM
                             "794.09"  #OTHER NONSPECIFIC ABNORMAL RESULTS OF FUNCTION STUDY OF BRAIN AND CENTRAL NERVOUS SYSTEM
)

is_MSexcl_code <- dx$ICD9_Code %in% ICD9_Codes_MSexcl
# Number of *different* Exclusive ICD9 codes per patients:
Table_ICD9_MSexcl <- tapply(is_MSexcl_code, dx$PAT_MRN_ID, function(codes) sum(unique(codes)))
Table_ICD9_MSexcl <- data.frame(PAT_MRN_ID=as.integer(rownames(Table_ICD9_MSexcl)),
                                MSexcl = c(Table_ICD9_MSexcl), 
                                stringsAsFactors = F)

is_MSexclG_code <- dx$ICD9_Code %in% ICD9_Codes_MSexclGeneral
# Number of *different* Highly Exclusive ICD9 codes per patients:
Table_ICD9_MSexclG <- tapply(is_MSexclG_code, dx$PAT_MRN_ID, function(codes) sum(unique(codes)))
Table_ICD9_MSexclG <- data.frame(PAT_MRN_ID=as.integer(rownames(Table_ICD9_MSexclG)), 
                                 MSexclG = c(Table_ICD9_MSexclG), 
                                 stringsAsFactors = F)

# Searching for MS medications --------------------------------------------

Medic_Names=c("INTERFERON BETA-1A", "AVONEX", "REBIF",
              "PEGINTERFERON BETA-1A","PLEGRIDY",
              "INTERFERON BETA-1B", "BETASERON", "EXTAVIA",
              "GLATIRAMER ACETATE", "GLATIRAMER","COPAXONE", 
              "NATALIZUMAB", "TYSABRI", 
              "TERIFLUNOMIDE", "AUBAGIO",
              "FINGOLIMOD","GILENYA", 
              "DIMETHYL FUMARATE", "TECFIDERA",
              "MITOXANTRONE","NOVANTRONE",
              "ALEMTUZUMAB","LEMTRADA")

is_matches <- grepl(paste(Medic_Names,collapse="|"), 
                    meds$Med_Desc, perl = TRUE)
#Number of MS medication per patient:
Table_Medic <- tapply(is_matches, meds$PAT_MRN_ID, sum)
Table_Medic <- data.frame(PAT_MRN_ID = as.integer(names(Table_Medic)), 
                          Nb_MS_Medic = c(Table_Medic), 
                          stringsAsFactors = F)


# Search for «Multiple Sclerosis» in clinical notes ---------------------

MS_MedPattern <- paste(c("MULTIPLE[[:space:]]{0,2}SCLEROSIS",
                         "CLINICALLY[[:space:]]{0,2}ISOLATED[[:space:]]{0,2}SYNDROME",
                         "([[:punct:]]|[[:space:]])(RRMS|SPMS|PPMS|PRMS|CDMS)([[:punct:]]|[[:space:]])",
                         "(RELAPSING|PROGRESSIVE|REMITTING|PROBABLE|DEFINITE)([[:space:]]{0,2}|\\-)MS([[:punct:]]|[[:space:]])", 
                         paste0(c("CRITERIA(S)?[[:space:]]{0,2}FOR",
                                  "(DIAGNOSIS|DX|DIAGNOSED|HISTORY)[[:space:]]{0,2}(OF|WITH)"),
                                "[[:space:]]{0,2}MS([[:punct:]]|[[:space:]])"),
                         "([[:punct:]]|[[:space:]])MS[[:space:]]{0,2}(DIAGNOSIS|DX|CENTER)",
                         "([[:space:]]|[[:punct:]])MS[[:space:]]{0,2}(RELATED|ATTACK|THERAP)",
                         "([[:space:]]|[[:punct:]])HA(S|VE)[[:space:]]{0,2}MS[[:space:]]{0,2}SINCE"),
                       collapse = "|")


n_matches <- sapply(gregexpr(MS_MedPattern, notes$NOTE_TEXT, ignore.case = T, perl = TRUE), function(x) if(is.na(x) || x[1] == -1) 0 else length(x))
MS_Text_total <- ddply(data.frame(PAT_MRN_ID = notes$PAT_MRN_ID, N = n_matches),
                       ~ PAT_MRN_ID,
                       summarize, 
                       MS_Text_total = sum(N),
                       MS_2_Notes = sum(N>=2),
                       MS_3_Notes = sum(N>=3),
                       MS_Notes_Max = max(N))


# Merge tables ------------------------------------------------------------

#List of tables to merge:
#classificationIds
#Table_ICD9_MS_cat
#Table_ICD9_MSexcl
#Table_Medic
#MS_Text_total

###Merging ID_APEX_Classification & Table_ICD9_MS_cat
stopifnot(Table_ICD9_MS_cat$PAT_MRN_ID %in% classificationIds$PAT_MRN_ID)
ALL_Table <- merge(classificationIds, Table_ICD9_MS_cat, by="PAT_MRN_ID", all.x = TRUE, all.y = TRUE)

###Merging ALL_Table and Table_ICD9_MSexcl
stopifnot(Table_ICD9_MSexcl$PAT_MRN_ID %in% ALL_Table$PAT_MRN_ID)
ALL_Table <- merge(ALL_Table, Table_ICD9_MSexcl, by="PAT_MRN_ID", all.x = TRUE, all.y = TRUE)
ALL_Table <- merge(ALL_Table, Table_ICD9_MSexclG, by="PAT_MRN_ID", all.x = TRUE, all.y = TRUE)

###Merging ALL_Table and Table_Medic
stopifnot(Table_Medic$PAT_MRN_ID %in% ALL_Table$PAT_MRN_ID)
ALL_Table <- merge(ALL_Table, Table_Medic, by="PAT_MRN_ID", all.x = TRUE, all.y = TRUE)

###Merging ALL_Table and MS_Text_total
stopifnot(MS_Text_total$PAT_MRN_ID %in% ALL_Table$PAT_MRN_ID)

ALL_Table <- merge(ALL_Table, MS_Text_total, by="PAT_MRN_ID", all.x = TRUE, all.y = TRUE)

# Check that we haven't added or removed any rows in the merging process:
stopifnot(nrow(classificationIds) == nrow(ALL_Table))


# Classification MS cases -------------------------------------------------

is.hit <- function(col) !(is.na(col) | col == 0)

Matching_if_2_MS <- !is.na(ALL_Table$MS_Text_total) & ALL_Table$MS_Text_total >= 2 # 2 or more matches
Matching_if_1_visit_with_2_MS <- is.hit(ALL_Table$MS_2_Notes) # 1 or more visit with two matches (more conservative)
print(table(Matching_if_2_MS, Matching_if_1_visit_with_2_MS))
cat(sprintf("Using '%s' as the final text matching for classification.\n", textMatchingStrategy <- "Matching_if_1_visit_with_2_MS"))
textMatching <- get(textMatchingStrategy)


# Final Classif -----------------------------------------------------------

ALL_Table$UCSFisWellDefGroup <- is.hit(ALL_Table$WellDefined) & 
  textMatching &
  !is.hit(ALL_Table$MSexcl)

ALL_Table$UCSFisProbGroupA <- is.hit(ALL_Table$ProbableA) &
  textMatching &
  is.hit(ALL_Table$Nb_MS_Medic) &
  !is.hit(ALL_Table$MSexcl)

ALL_Table$UCSFisProbGroupB <-  is.hit(ALL_Table$ProbableB) &
  textMatching &
  !is.hit(ALL_Table$MSexcl)

ALL_Table$UCSFisProbGroupC <-  textMatching &
  !is.hit(ALL_Table$MSexcl)

ALL_Table$UCSFClassification <- ifelse(ALL_Table$UCSFisWellDefGroup,
                                       "WellDefined_MS_Group",
                                       ifelse(ALL_Table$UCSFisProbGroupA,
                                              "Probable_MS_Group_A",
                                              ifelse(ALL_Table$UCSFisProbGroupB,
                                                     "Probable_MS_Group_B",
                                                     ifelse(ALL_Table$UCSFisProbGroupC,
                                                            "Probable_MS_Group_C",
                                                            "Unclassified"))))

cat("Final Classification results:\n")
print(table(ALL_Table$UCSFClassification, useNA="ifany")) #check
groups <- ddply(ALL_Table, c("UCSFisWellDefGroup", "UCSFisProbGroupA", "UCSFisProbGroupB", "UCSFisProbGroupC"), nrow)
colnames(groups)[5] <- "N"
print(groups[order(groups$N, decreasing = TRUE),])

Patients_Classification <- ALL_Table[,c("PAT_MRN_ID", "UCSFClassification")]

cat("\n Identification and classification of MS patients done\n\n")
