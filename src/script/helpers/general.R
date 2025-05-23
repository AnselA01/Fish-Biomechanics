library(stringi)
 
# arg name: expects subject name as a string in the form "<number><segment><trial>" (no < or >)
# returns list of fish type, number, segment, and trial
parseSubjectName <- function(subject.name) {
  fish.type <- str_sub(subject.name, end = 2)
  subject.name <- substring(subject.name, 3)
  fish.number <- parse_number(subject.name)
  segment <- str_extract(subject.name, "[a-zA-Z]+")
  trial <- parse_number(str_remove(stri_reverse(subject.name), "^0"))
  return(list(
    fish.type = fish.type,
    fish.number = fish.number,
    segment = segment,
    trial = trial
  ))
}

# something is up with this
# gets name of subject from bone df
getName <- function(bone, sep = "") {
  return(paste(toupper(bone$Individual[[1]]), bone$Segment[[1]], bone$Trial[[1]], sep = sep))
}

# standardizes name like PF01CP1 to the correct file name pf01cp01.
# used only when copying raw bone data files
standardize_name <- function(name) {
  name <- tolower(name)
  prefix <- substr(name, 1, nchar(name) - 1)      
  num <- sub(".*?(\\d{1,2})$", "\\1", name)    
  num_padded <- sprintf("%02d", as.integer(num))
  return(paste0(substr(name, 1, nchar(name) - nchar(num)), num_padded))
}

# extracts bone identifer metadata into a named list 
# arg: bone identifier name
# returns named list of three: individual (pf01), segment (ut, mt, lt, or cp), trial number (01)
extract_metadata <- function(boneId) {
  identifiers <- str_match(boneId, "^([a-zA-Z]{2,}\\d{2,})([a-zA-Z]{2,})(\\d{2,})")
  return(
    list(
      individual = identifiers[2],
      segment = toupper(identifiers[3]),
      trial = as.character(as.integer(identifiers[4]))
    )
  )
}

# is bone metadata found in the area data provided in the "data/area.csv" folder?
# arg metadata: a vector of three bone identifiers: segment (pf01), segment (UT), trial # (01)
# arg area_identifiers: a dataframe with three columns.
# returns whether the vector matches a row in area_identifiers
bone_is_in_area_data <- function(metadata, area_identifiers) {
  return(any(apply(area_identifiers, 1, function(row) all(row == metadata))))
}

