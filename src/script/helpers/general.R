library(stringi)
 
# arg name: expects subject name as a string in the form "<number><segment><trial>" (no < or >)
# returns list of fish type, number, segment, and trial
parseSubjectName <- function(subject.name) {
  fish.type <- str_sub(subject.name, end = 2)
  subject.name <- substring(subject.name, 3)
  fish.number <- parse_number(subject.name)
  segment <- str_extract(subject.name, "[a-zA-Z]+")
  trial <- parse_number(str_remove(stri_reverse(subject.name), "^0"))
  return(list(fish.type = fish.type, fish.number = fish.number, segment = segment, trial = trial))
}

# something is up with this
# gets name of subject from bone df
getName <- function(bone, sep = "") {
  return(paste(toupper(bone$Individual[[1]]), bone$Segment[[1]], bone$Trial[[1]], sep = sep))
}

# standardizes name like PF01CP1 to the correct filename pf01cp01
standardize_name <- function(name) {
  name <- tolower(name)
  prefix <- substr(name, 1, nchar(name) - 1)         
  num <- sub(".*?(\\d{1,2})$", "\\1", name)    
  num_padded <- sprintf("%02d", as.integer(num))
  return(paste0(substr(name, 1, nchar(name) - nchar(num)), num_padded))
}