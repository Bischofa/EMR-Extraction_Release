
#is.empty----
#Function that checks if output of a gregexpr item is empty or not
is.empty <- function(x, input = m){
  if(length(input[[x]]) > 1){
    return(FALSE)
  }
  else {
    bool <- (input[[x]] == -1)
    return(bool)
  }
}

#Removing duplicated rows
removeDuplicates <- function(v, dfName = deparse(substitute(v))) {
  cat("# Checking duplicated rows in", dfName," \n")
  dups <- duplicated(v)
  if (sum(dups) > 0) {
    cat(paste("INPUT_ARGS_WARNING:", dfName, "has", sum(dups), "duplicated rows. Investigate!...\n"))
    return(if(is.vector(v)) v[!dups] else v[!dups,])
  } else {
  cat("No duplicated rows in", dfName," \n")
  return(v)
  }
}


#extract_EDSS_values
#Function that reads in numeric text string and coerces numerics
extract_EDSS_values <- function(string = values){
  
  replace_string <- string
  replace_string <- gsub("[\\,\\.][[:space:]]*$|([\\.\\,])|[[:punct:]]|[[:space:]]", "\\1", replace_string)
  replace_string <- sub("ZERO","0",replace_string)
  replace_string <- sub("ONE","1",replace_string)
  replace_string <- sub("TWO","2",replace_string)
  replace_string <- sub("THREE","3",replace_string)
  replace_string <- sub("FOUR","4",replace_string)
  replace_string <- sub("FIVE","5",replace_string)
  replace_string <- sub("SIX","6",replace_string)
  replace_string <- sub("SEVEN","7",replace_string)
  replace_string <- sub("EIGHT","8",replace_string)
  replace_string <- sub("NINE","9",replace_string)
  replace_string <- sub("TEN","10",replace_string)
  
  return(as.numeric(replace_string))
}

#extract_walk_values
#Function that reads in numeric text string and coerces numerics
extract_walk_values <- function(string = values){
  
  replace_string <- string
  replace_string <- gsub("S","",replace_string)
  replace_string <- gsub("([[:punct:]]|[[:space:]])*$|^([[:punct:]]|[[:space:]])*","",replace_string)
  return(as.numeric(replace_string))
}

#regmatches2 
#function that takes gregexpr or regexpr output and returns the text match,
#along with X characters back and X characters front
regmatches2 <- function (x, m, back, front, invert = FALSE) 
{
  if (length(x) != length(m)) 
    stop(gettextf("%s and %s must have the same length", 
                  sQuote("x"), sQuote("m")), domain = NA)
  ili <- is.list(m)
  useBytes <- if (ili) 
    any(unlist(lapply(m, attr, "useBytes")))
  else any(attr(m, "useBytes"))
  if (useBytes) {
    asc <- iconv(x, "latin1", "ASCII")
    ind <- is.na(asc) | (asc != x)
    if (any(ind)) 
      Encoding(x[ind]) <- "bytes"
  }
  if (!ili && !invert) {
    so <- m[ind <- (!is.na(m) & (m > -1L))]
    eo <- so + attr(m, "match.length")[ind] - 1L
    return(substring(x[ind], so, eo))
  }
  y <- if (invert) {
    Map(function(u, so, ml) {
      if ((n <- length(so)) == 1L) {
        if (is.na(so)) 
          return(character())
        else if (so == -1L) 
          return(u)
      }
      beg <- if (n > 1L) {
        eo <- so + ml - 1L
        if (any(eo[-n] >= so[-1L])) 
          stop(gettextf("need non-overlapping matches for %s", 
                        sQuote("invert = TRUE")), domain = NA)
        c(1L, eo + 1L)
      }
      else {
        c(1L, so + ml)
      }
      end <- c(so - 1L, nchar(u))
      substring(u, beg, end)
    }, x, m, if (ili) 
      lapply(m, attr, "match.length")
    else attr(m, "match.length"), USE.NAMES = FALSE)
  }
  else {
    Map(function(u, so, ml) {
      if (length(so) == 1L) {
        if (is.na(so) || (so == -1L)) 
          return(character())
      }
      substring(u, so - back  , so + ml + front - 1L)
    }, x, m, lapply(m, attr, "match.length"), USE.NAMES = FALSE)
  }
  names(y) <- names(x)
  y
}

getFirstHit <- function(matchIndexes, inputData, offset) {
  # getFirstHit is a hit selection function that returns
  # the first match after the anchor if there is any, or NA
  # if there isn't.
  # It expects the output of a grexepr as a first argument
  output <- regmatches(inputData, matchIndexes)
  sapply(1:length(inputData), function(i) {
    output[[i]][matchIndexes[[i]]>offset][1] # Yes baby
  })
}

getClosestHit <- function(matchIndexes, inputData, offset) {
  # getClosestHit is a hit selection function that returns
  # the closest match to the anchor if there is any, or NA
  # if there isn't.
  # It expects the output of a grexepr as a first argument
  if (length(offset)) {
    offset <- rep(offset, length(inputData))
  }
  output <- regmatches(inputData, matchIndexes)
  sapply(1:length(inputData), function(i) {
    output[[i]][order(abs(matchIndexes[[i]] - offset[i]))[1]] 
  })
}

swap <- function(array, i, j) {
  if (length(i) == 0 || length(j) == 0 || is.na(j) || is.na(i)) {
    return(array)
  }
  if (i==j) {
    return(array)
  }
  tmp <- array[i]
  array[i] <- array[j]
  array[j] <- tmp
  return(array)
}


# EMR_Validation Functions ------------------------------------------------

# this function is meant to take in a set of EDSS belonging to a single patient 
# and the corresponding visit order. returns melted visit orders for equal EDSS's
replace_visit_order <- function(EDSS ) {
  visit_order <- 1:length(EDSS)
  EDSS_levels <- unique(EDSS)
  for(i in 1:length(EDSS_levels)) {
    index <- which(EDSS == EDSS_levels[i])
    replace_visit <- paste0(visit_order[index], collapse = ",")
    visit_order[index] <- replace_visit
  }
  return(visit_order)
}



# Handling of the TABLES object -------------------------------------------

putInGlobalEnv <- function(LIST) {
  dummy <- sapply(names(LIST), function(objectName) assign(objectName, LIST[[objectName]], envir = .GlobalEnv))
  invisible()
}
