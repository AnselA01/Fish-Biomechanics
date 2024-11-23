library(stringi)
 
# arg name: expects subject name as a string in the form "<number><segment><trial>" (no < or >)
gen.parseSubjectName <- function(subject.name) {
  # remove pf or whatever from the front. you can pass it if it makes you happy
  fish_number <- parse_number(str_remove(subject.name, "^0")) # extract first number and remove leading 0 if present
  segment <- gsub("[^a-zA-Z]", "", x = subject.name)
  trial <- parse_number(str_remove(stri_reverse(subject.name), "^0")) # extract first number after reversal. leading 0 again
  return(c(fish.number = fish_number, segment = segment, trial = trial))
}