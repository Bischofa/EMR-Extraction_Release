# EMR_Extraction.R 
# This script performs the extraction of MS realted metrics from the text of the notes.
#
# Copyright 01/15 Antoine Lizee, Vincent Damotte, Wendy Tang, Tiffany Chen. antoine.lizee@ucsf.edu

#In EMR dataset, to keep only patients that have been classified into one of the 4 classification groups
definedMRN <- Patients_Classification$PAT_MRN_ID[!Patients_Classification$UCSFClassification == "Unclassified"]
notes <- TABLES$notes[ TABLES$notes$PAT_MRN_ID %in% definedMRN, ]

# Small operations on the notes -------------------------------------------

# Get rid of non alphabet, non digit characters
notes$NOTE_TEXT <- gsub("[^[:alnum:][:space:]\\.\\,\\-\\[\\]\\;\\_\\:]","", notes$NOTE_TEXT)


# Change gregexpr default behavior ----------------------------------------

gregexpr <- function(...) {
  args = list(...)
  if (!"ignore.case" %in% names(args)) args <- c(args, list(ignore.case = TRUE))
  do.call(base::gregexpr, args)
}

gsub <- function(...) {
  args = list(...)
  if (!"ignore.case" %in% names(args)) args <- c(args, list(ignore.case = TRUE))
  do.call(base::gsub, args)
}


# EDSS Extraction: -----------------------------------------------------------
cat("\n# 04.02.01. EDSS Extraction\n")

EDSS_anchor_pattern <- "(EDSS|EXPANDED DISABILITY .{0,15} SCORE|DISABILITY SCORE|DISABILITY OF )"
EDSS_pattern <- paste0("([[:punct:]]|[[:space:]])",
                       "([[:digit:]](\\.[05])?|[0]?\\.5|ZERO|ONE|TWO|THREE|FOUR|FIVE|SIX|SEVEN|EIGHT|NINE|TEN)", #EDSS.anchored
                       "([[:punct:]]|[[:space:]])")
n.backwardSelect <- 40
n.forwardSelect <- 150
n.forwardSelect2 <- 70 

# EDSS Phase 1: ----------------------------------------------------------------
cat("# 04.02.01.01. EDSS Extraction Phase 1: extraction of the value(s)\n")
# Extract lines that mention EDSS
EDSS.m <- gregexpr(pattern=EDSS_anchor_pattern, notes$NOTE_TEXT, perl = TRUE)
EDSS.lines  <- regmatches2(notes$NOTE_TEXT, EDSS.m , back=n.backwardSelect, front=n.forwardSelect)

# Create the normalized table with the anchored text
EDSS.matches <- do.call(rbind, lapply(1:nrow(notes), function(i) data.frame(note.index = rep(i, length(EDSS.lines[[i]])),  
                                                                            anchored = EDSS.lines[[i]], stringsAsFactors = F)) )

# Now we proceed to extract value of EDSS
EDSS.anchored <- substring(EDSS.matches$anchored, n.backwardSelect, nchar(EDSS.matches$anchored))
# First, we extract values that have total in front of them. If there is a hit, replace the anchored.
EDSS.pat.TOTAL <- paste0(".*TOTAL[[:space:]]*(EDSS)?[[:space:]]*(OF)?([[:space:]]|[[:punct:]])*(", EDSS_pattern, ").*")
EDSS.anchored.processed <- gsub(EDSS.pat.TOTAL, "\\4", EDSS.anchored, perl = TRUE) 
# Second, we exclude ranges, that we don't support yet.
EDSS.pat.RANGE <- "([[:punct:]]|[[:space:]])[[:digit:]](\\.(0|5))?([-[:space:]]){1,2}[[:digit:]](\\.[0|5])?([[:punct:]]|[[:space:]])"
EDSS.anchored.processed[grepl(EDSS.pat.RANGE, EDSS.anchored.processed, perl = TRUE)] <- ""
#Then, wee look for hits
EDSS.anchored.processed <- substring(EDSS.anchored.processed, 0, n.forwardSelect2)
EDSS.m2 <- gregexpr(pattern = EDSS_pattern, EDSS.anchored.processed, perl = TRUE) 
# We select the best hit depending on its position from the anchor...
EDSS.matches$hit <- getFirstHit(EDSS.m2, EDSS.anchored.processed, 0)

# Read in and interpret values of EDSS 
EDSS.matches$value <- extract_EDSS_values(string=EDSS.matches$hit)
EDSS.matches <- EDSS.matches[,c(1,4,3,2)]


# EDSS Denormalization ----------------------------------------------------

cat("\n# 04.02.01.02. EDSS Extraction Phase 2: Denormalization\n")
#Merge the several hits per index
EDSS.matches.denorm <- ddply(EDSS.matches, ~ note.index, function(dfi) {
  values <- dfi$value
  
  # We make the decision here
  val.index <- which(values == suppressWarnings(max(values, na.rm = T)) )[1]
  # We create the line
  if (is.na(val.index)) { #Case with some anchors but no hits.
    data.frame(nAnchored = nrow(dfi), 
               nHits = sum(!is.na(values)),
               ambiguous = NA,
               value = NA,
               values = NA,
               anchored = dfi$anchored[1],
               anchored2 = dfi$anchored[2],
               anchored3 = dfi$anchored[3])
  } else {
    anchoreds <- swap(dfi$anchored, 1, val.index)
    values <- swap(dfi$value, 1, val.index)
    data.frame(nAnchored = nrow(dfi), 
               nHits = sum(!is.na(values)),
               ambiguous = as.logical(length(table(values))>1),
               value = max(values, na.rm=TRUE),
               values = paste(values, collapse = ", "),
               anchored = anchoreds[1],
               anchored2 = anchoreds[2],
               anchored3 = anchoreds[3])
  }
})

EDSS.final <- data.frame(PAT_MRN_ID = notes$PAT_MRN_ID, NOTE_ID = notes$NOTE_ID,  CONTACT_DATE = notes$CONTACT_DATE, note_type = notes$note_type, EDSS.matches.denorm[match(1:nrow(notes), EDSS.matches.denorm$note.index),])[,-5]

cat(" Extraction of EDSS values completed\n\n")

# 25ft Extraction: -----------------------------------------------------------
cat("\n# 04.02.02. Timed 25 Foot Walk Extraction\n")

# Extract lines that mention 25 Foot Walk

walk_anchor_pattern <- "(25|TWENTY[[:space:]\\-]{0,2}FIVE)[[:space:]\\-]?(F[OE]{2}T|FT|FW|F W)|WALK(ING)?[[:space:]]{0,2}TIME"
float_pattern <- "[0-9]{1,2}(\\.[0-9]{0,3})?"
walk_pattern <- paste0("[[:space:][:punct:]]", float_pattern, "([[:space:][:punct:]]|S)")
walk_assistance <- "WITH[[:space:]]((A|AN|(UNI|BI)LATERAL|HIS|HER|SOME|ONE|TWO)[[:space:]])?(WALKER|CANE|CRUTCH|SUPPORT|ASSISTANCE|AID)"
n.backwardSelect <- 40
n.forwardSelect <- 60

cat("\n# 04.02.02.01. T25FW Extraction Phase 1: extraction of the value(s)\n")

walk.m <- gregexpr(pattern=walk_anchor_pattern, notes$NOTE_TEXT, perl = TRUE)
walk.lines  <- regmatches2(notes$NOTE_TEXT, walk.m , back=n.backwardSelect, front=n.forwardSelect)

# Create the normalized table with the anchored text
walk.matches <- do.call(rbind, lapply(1:nrow(notes), function(i) data.frame(note.index = rep(i, length(walk.lines[[i]])),  
                                                                            anchored = walk.lines[[i]],
                                                                            stringsAsFactors = FALSE)))

# Now we proceed to extract value of walk
walk.anchored <- walk.matches$anchored
# Remove dates 
walk.pat.DATE <- "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}"
walk.anchored.processed <- gsub(walk.pat.DATE, "", walk.anchored, perl = TRUE) 
# Extract values that are a range and replace them by the first one.
walk.pat.RANGE <- paste0("[[:space:][:punct:]](", float_pattern, ")[[:space:][:punct:]]?([/\\-]|AND)[[:space:][:punct:]]?(", float_pattern, ")[[:space:][:punct:]]")
walk.anchored.processed <- gsub(walk.pat.RANGE, " \\4 ", walk.anchored.processed, perl = TRUE) # \\1 for the first, \\4 for the second pattern.
#Locate the hits
walk.m2 <- gregexpr(pattern=walk_pattern, walk.anchored.processed, perl = TRUE) 
# Now we select the best hit depending on its position from the anchor
walk.matches$hit <- getFirstHit(walk.m2, walk.anchored.processed, n.backwardSelect)

# Read in and interpret values of walk 
walk.matches$value <- extract_walk_values(string=walk.matches$hit)
walk.matches <- walk.matches[,c(1,4,3,2)]

# Look for assistance
walk.matches$assistance <- grepl(walk_assistance, substr(walk.matches$anchored, n.backwardSelect, nchar(walk.matches$anchored)),
                                 ignore.case = TRUE, perl = TRUE)

# walk Denormalization ----------------------------------------------------
cat("\n# 04.02.02.02. T25FW Extraction Phase 2: Denormalization\n")

#Merge the several hits per index
walk.matches.denorm <- ddply(walk.matches, ~ note.index, function(dfi) {
  values <- dfi$value
  assistances <- dfi$assistance
  # We make the decision here
  assistance <- names(sort(table(assistances), decreasing = T))[1]
  anchoreds <- dfi$anchored
  value <- suppressWarnings(mean(values, na.rm = T))
  # We create the line
  if (is.na(value)) { #Case with some anchors but no hits.
    data.frame(nAnchored = nrow(dfi), 
               nHits = sum(!is.na(values)),
               ambiguous = NA,
               value = NA,
               values = NA,
               anchored = dfi$anchored[1],
               anchored2 = dfi$anchored[2],
               anchored3 = dfi$anchored[3])
  } else {
    data.frame(nAnchored = nrow(dfi), 
               nHits = sum(!is.na(values)),
               ambiguous = as.logical(length(table(values))>1),
               value = value,
               values = paste(values, collapse = ", "),
               assistance = assistance,
               assistances = paste(assistances, collapse = ", "),
               anchored = anchoreds[1],
               anchored2 = anchoreds[2],
               anchored3 = anchoreds[3])
  }
})

walk.final <- data.frame(PAT_MRN_ID = notes$PAT_MRN_ID, NOTE_ID = notes$NOTE_ID,  CONTACT_DATE = notes$CONTACT_DATE, note_type = notes$note_type, walk.matches.denorm[match(1:nrow(notes), walk.matches.denorm$note.index),])[,-5]

cat(" Extraction of T25FW values completed\n\n")

# Subtype Extraction ------------------------------------------------------
cat("\n# 04.02.03. MS Subtype Extraction\n")

subtype.patterns <- list(RR = "RR(MS|[[:space:]]{0,2}MULTIPLE[[:space:]]{0,2}SCLEROSIS)|RELAPSING([[:space:]]{0,2}|\\-|[[:space:]]AND[[:space:]])REMITTING", 
                         SP = "SP(MS|[[:space:]]{0,2}MULTIPLE[[:space:]]{0,2}SCLEROSIS)|(SECONDARY|SECONDARILY)([[:space:]]{0,2}|\\-)PROGRESSIVE",
                         PP = "PP(MS|[[:space:]]{0,2}MULTIPLE[[:space:]]{0,2}SCLEROSIS)|(PRIMARY|PRIMARILY)([[:space:]]{0,2}|\\-)PROGRESSIVE",
                         PR = "PR(MS|[[:space:]]{0,2}MULTIPLE[[:space:]]{0,2}SCLEROSIS)|PROGRESSIVE([[:space:]]{0,2}|\\-)RELAPSING([[:space:]]{0,2}|\\-)",
                         P = "[[:space:]](P)(MS|[[:space:]]{0,2}MULTIPLE[[:space:]]{0,2}SCLEROSIS)|[[:space:]]PROGRESSIVE([[:space:]]{0,2}|\\-)(MS|MULTIPLE[[:space:]]{0,2}SCLEROSIS)",
                         CIS = "CLINICALLY([\\-]|[[:space:]])ISOLATED([\\-]|[[:space:]])SYNDROME|[[:space:]]CIS([[:punct:]]|[[:space:]])")

n.backwardSelect <- 70
n.forwardSelect <- 40

cat("\n# 04.02.03.01. MS Subtype Extraction Phase 1: extraction of the value(s)\n")

subtypes.ms <- lapply(subtype.patterns, function(pat) gregexpr(pat, notes$NOTE_TEXT, perl = TRUE))
subtype.lines  <- lapply(subtypes.ms, function(subtypes.m) regmatches2(notes$NOTE_TEXT, subtypes.m , back=n.backwardSelect, front=n.forwardSelect))

isPPorSP <- sapply(subtype.lines$SP, function(match) length(match) != 0) | sapply(subtype.lines$PP, function(match) length(match) != 0)
subtype.lines$P[isPPorSP] <- list(character(0)) 

# Create a temporary normalized table with the anchored text
subtype.matches <- melt(lapply(subtype.lines,
                               function(subtype.lines.i)
                                 do.call(rbind, lapply(1:nrow(notes), function(i) data.frame(note.index = rep(i, length(subtype.lines.i[[i]])),  
                                                                                             anchored = subtype.lines.i[[i]]) ))
),
id = 1:2)

names(subtype.matches)[names(subtype.matches) == "L1"] <- "value"
subtype.matches$value <- factor(subtype.matches$value, levels = names(subtype.patterns))


# Subtypes denormalisation ------------------------------------------------
cat("\n# 04.02.03.02. MS Subtype Extraction Phase 2: Denormalization\n")

#Merge the several hits per index
subtype.matches.denorm <- ddply(subtype.matches, ~ note.index, function(dfi) {
  values <- dfi$value
  # We make the decision here
  ordered.index <- order(-table(values)[as.numeric(values)], values) #order by the most seen, then by the order of pattern definition
  anchored <- dfi$anchored[ordered.index]
  # We create the line
  data.frame(nAnchored = nrow(dfi), 
             value = values[ordered.index][1],
             nHits = sum(!is.na(values)),
             ambiguous = as.logical(length(table(as.character(values)))>1),
             values = paste(values[ordered.index], collapse = ", "),
             anchored  = anchored[1],
             anchored  = anchored[2],
             anchored  = anchored[3])
})

subtype.final <- data.frame(PAT_MRN_ID = notes$PAT_MRN_ID, NOTE_ID = notes$NOTE_ID,  CONTACT_DATE = notes$CONTACT_DATE, note_type = notes$note_type, 
                            subtype.matches.denorm[match(1:nrow(notes), subtype.matches.denorm$note.index),])[,-5]

cat(" Extraction of MS Subtypes completed\n\n")


# Age of Onset ------------------------------------------------------------

cat("\n# 04.02.04. Onset Extraction\n")

MS_pat <- "([[:space:][:punct:]](SP|P|RR|PP)?MS[[:space:]]|MULTIPLE(\\-|[[:space:]])SCLEROSIS)"
in_pat <- function(n = 50) sprintf("([^\\.]{0,%d}[[:space:]])?(in|since)[[:space:]]", n)

onset_anchor_pattern <- paste("DIAGNOSIS:[[:space:]]", # "ONSET:[[:space:]]"
                              paste0("(DIAGNOS|ONSET).{0,30}", MS_pat, in_pat()),
                              paste0(MS_pat,".{0,30}(DIAGNOS|ONSET)", in_pat(10)),
                              paste0("([[:space:]]HX[[:space:]]|HISTORY).{0,6}", MS_pat),
                              paste0(MS_pat,".{0,6}([[:space:]]HX[[:space:]]|HISTORY)"),
                              "FIRST.{0,20}SYMPTOM",
                              paste0("SYMPTOM(S)[[:space:]](STARTED|BEG[AI]N)", in_pat(5)),
                              "((NORMAL.{0,30}|STATE[[:space:]]OF[[:space:]])HEALTH|HEALTHY|[[:space:]]WELL[[:space:]]).{0,20}UNTIL",
                              sep = "|")

year_pattern <- "(19[[:digit:]][[:digit:]]|2(0|1)[[:digit:]][[:digit:]])"
n.backwardSelect <- 50
n.forwardSelect <- 100

cat("\n# 04.02.04.01. Onset Extraction Phase 1: extraction of the values\n")

onset.m <- gregexpr(pattern=onset_anchor_pattern, notes$NOTE_TEXT, perl = TRUE)
onset.lines  <- regmatches2(notes$NOTE_TEXT, onset.m , back=n.backwardSelect, front=n.forwardSelect)

# Create the normalized table with the anchored text
onset.matches <- do.call(rbind, lapply(1:nrow(notes), function(i) data.frame(note.index = rep(i, length(onset.lines[[i]])),  
                                                                             anchored = onset.lines[[i]], 
                                                                             stringsAsFactors = F)))

onset.anchored <- onset.matches$anchored
# trim down highly specific patterns
onset.pat.SPEC <- ".*(DIAGNOSIS:.{15}).*" # "ONSET:[[:space:]]"
onset.anchored.processed <- gsub(onset.pat.SPEC, "\\1", onset.anchored, perl = TRUE) 
# Now we proceed to extract value of onset time
onset.m2 <- gregexpr(pattern=year_pattern, onset.anchored.processed, perl = TRUE) 

onset.matches$hit <- getClosestHit(onset.m2, onset.anchored.processed, nchar(onset.anchored.processed) - n.forwardSelect) 

# Read in and interpret values of onset
onset.matches$value <- as.numeric(onset.matches$hit)

onset.matches <- onset.matches[,c(1,4,3,2)]

cat("\n# 04.02.04.02. Onset Extraction Phase 2: Denormalization\n")

onset.matches.denorm <- ddply(onset.matches, ~ note.index, function(dfi) {
  values <- dfi$value
  # We make the decision here
  val.index <- which(values == suppressWarnings(min(values, na.rm = T)) )[1]  #Took minimum
  # We create the line
  if (is.na(val.index)) { #Case with some anchors but no hits.
    data.frame(nAnchored = nrow(dfi), 
               nHits = sum(!is.na(values)),
               ambiguous = NA,
               value = NA,
               values = NA,
               anchored = dfi$anchored[1],
               anchored2 = dfi$anchored[2],
               anchored3 = dfi$anchored[3])
  } else {
    anchoreds <- swap(dfi$anchored, 1, val.index)
    values <- swap(dfi$value, 1, val.index)
    data.frame(nAnchored = nrow(dfi), 
               nHits = sum(!is.na(values)),
               ambiguous = as.logical(length(table(values))>1),
               value = values[1],
               values = paste(values, collapse = ", "),
               anchored = anchoreds[1],
               anchored2 = anchoreds[2],
               anchored3 = anchoreds[3])
  }
})

onset.final <- data.frame(PAT_MRN_ID = notes$PAT_MRN_ID, NOTE_ID = notes$NOTE_ID, CONTACT_DATE = notes$CONTACT_DATE, note_type = notes$note_type, 
                          onset.matches.denorm[match(1:nrow(notes), onset.matches.denorm$note.index),])[,-5]

cat(" Extraction of Onset completed\n\n")



cat(" Extraction of unstructured done\n\n")



