# A wrapper around youngsModulus.R and youngsModulusDetermination.R to 
# perform Young's Modulus calculation and decision making for all data in
# the "data" directory

current_date <- NULL

automated_youngs_modulus_calculation <- function() {
  source("./src/script/helpers/data.R")
  source("./src/script/helpers/general.R")
  
  bones <- data.generator()
  
  choices <- tibble()
  results <- tibble()
  for (bone in bones) {
    res <- calculate_result_and_choose(bone)
    if (is.null(res)) {
      message(paste0(getName(bone), ": Could not calculate Young's Modulus. Skipping"))
      next
    }
    
    choices <- bind_rows(choices, res$choice)
    results <- bind_rows(results, res$result)
  }
  
  results_and_choices <- list(results = results, choices = choices)
  
  save_results_choices_inconclusives(results_and_choices)
}

calculate_result_and_choose <- function(bone) {
  result <- ym.calculateAndDetermine(bone)
  if (is.null(result$results) || is.null(result$choice)) {
    return(NULL)
  }
  
  return(list(result = extract_result(result$results), choice = extract_choice(result$choice)))
}

extract_choice <- function(choice) {
  name <- choice$name[[1]]
  method <- choice$method[[1]]
  slope <- choice$slope[[1]]
  score <- choice$score[[1]]
  strain <- choice$strain[[1]]
  inconclusive <- choice$inconclusive
  
  return(
    tibble(
      name = name,
      method = method,
      slope = slope,
      score = score,
      strain = strain,
      inconclusive = inconclusive
    )
  )
}

# extracts and returns relevant values from young's modulus result
extract_result <- function(result) {
  names <- result$name
  rsquared <- result$r.squared
  max.slope <- result$max.slope
  max.strain <- result$max.strain
  max.score <- result$max.score
  inflection.slope <- result$inflection.slope
  inflection.strain <- result$inflection.strain
  inflection.score <- result$inflection.score
  fds.slope <- result$fds.slope
  fds.strain <- result$fds.strain
  fds.score <- result$fds.score
  
  return(
    tibble(
      bone_name,
      max.slope,
      max.strain,
      max.score,
      inflection.slope,
      inflection.strain,
      inflection.score,
      fds.slope,
      fds.strain,
      fds.score
    )
  )
}

# creates a directory `path` if it does not exist
create_dir_safe <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path)
  }
}

save_results_choices_inconclusives <- function(results_and_choices) {
  current_date <<- Sys.Date()
  
  output_dir <- file.path("results", "youngs-modulus", as.character(current_date))
  create_dir_safe(output_dir)
  
  message("Saving results to ", output_dir)
  
  inconclusive_data_dir <- file.path(output_dir, "inconclusive-data")
  create_dir_safe(inconclusive_data_dir)
  
  # passing date to all three is largely unnecessary but you never know, this could be run at 11:59:59
  save_results(results_and_choices$results, output_dir) 
  save_choices(results_and_choices$choices, output_dir)
  save_inconclusives(results_and_choices$choices %>% 
                       dplyr::filter(inconclusive) %>% 
                       dplyr::pull(bone_name),
                     output_dir,
                     inconcslusive_data_dir
                     )
}

save_results <- function(results, output_results_dir) {
  write_csv(results, file.path(output_results_dir, "results.csv"))
}

save_choices <- function(choices, output_choices_dir) {
  write_csv(choices, file.path(output_choices_dir, "choices.csv"))
}
# saves inconclusive names to text file and copies original data to inconclusive folder
save_inconclusives <- function(names, output_dir, output_inconclusives_dir) {
  if (!length(inconclusives)) { # ha, as if
    message("No inconclusive choices! Yippeee!!")
    return()
  }
  
  copy_inconclusives(names, output_inconclusives_dir)
  
  text <- c(paste("Generated on:", Sys.time()), names)
  inconclusives_txt_path <- file.path(output_dir, "inconclusives.txt")
  writeLines(text, inconclusives_txt_path)
}

# copies raw data for bones marked inconclusive. May he helpful?
# NOTE - will only work if bone data CSVs are named correctly. there is no way around that.
copy_inconclusives <- function(bone_names, inconclusive_data_dir) {
  for (bone_name in bone_names) {
    # find directory for bone name
    dirname <- tolower(str_sub(bone_name, 0, 4))
    filename <- paste0(standardize_name(bone_name), ".csv")
    # find file name for bone name
    raw_data_path <- file.path("data", dirname, filename)

    file.copy(raw_data_path, inconclusive_data_dir)
  }
}



