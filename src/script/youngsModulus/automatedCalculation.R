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
      name,
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

save_results_choices_inconclusives <- function(results_and_choices) {
  current_date <<- Sys.Date()
  message("Saving results to ", file.path("results", "youngs-modulus", current_date))
  output_dir <- file.path("results", "youngs-modulus", as.character(current_date))
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  inconclusive_data_dir <- file.path(output_dir, "inconclusive-data")
  if (!dir.exists(inconclusive_data_dir)) {
    dir.create(inconclusive_data_dir)
  }
  
  # passing date to all three is largely unnecessary but you never know, this may be run at 11:59:59
  save_results(results_and_choices$results) 
  save_choices(results_and_choices$choices)
  save_inconclusives(results_and_choices$choices %>% dplyr::filter(inconclusive) %>% dplyr::pull(name))
}

save_results <- function(results) {
  write_csv(results, file.path("results", "youngs-modulus", current_date, "results.csv"))
}

save_choices <- function(choices) {
  write_csv(choices, file.path("results", "youngs-modulus", current_date, "choices.csv"))
}
# saves inconclusive names to text file and copies original data to inconclusive folder
save_inconclusives <- function(names) {
  copy_inconclusives(names)
  
  if (!length(inconclusives)) { # ha, as if
    message("No inconclusive choices! Yippeee!!")
  }
  
  text <- c(paste("Generated on:", Sys.time()), names)
  writeLines(text, file.path("results", "youngs-modulus", current_date, "inconclusives.txt"))
}

# copies raw data for bones marked inconclusive. May he helpful?
copy_inconclusives <- function(names) {
  for (name in names) {
    # find directory for name
    dirname <- tolower(str_sub(name, 0, 4))
    filename <- paste0(standardize_name(name), ".csv")
    # find file name for name
    raw_data_path <- file.path("data", dirname, filename)
    copy_path <- file.path("results", "youngs-modulus", current_date, "inconclusive-data")
    
    file.copy(raw_data_path, copy_path)
  }
}



