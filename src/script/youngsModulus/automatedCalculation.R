# A wrapper around youngsModulus.R and youngsModulusDetermination.R to 
# perform Young's Modulus calculation and decision making for all data in
# the "data" directory


# in case only the automated function is run
load_necessary_files <- function() {
  library(readr)
  library(tibble)
  
  source("./src/script/helpers/data.R")
  source("./src/script/helpers/general.R")
  source("./src/script/youngsModulus/youngsModulus.R")
  source("./src/script/youngsModulus/youngsModulusDetermination.R")
}

current_date <- NULL

automated_youngs_modulus_calculation <- function(bone_data) {
  load_necessary_files()
  tryCatch({
    bone_data <- handle_data_fetch(bone_data)
  }, error = function(e) {
    stop("Error fetching data. Stopping", e)
  })
  
  results_and_choices <- NULL
  tryCatch({
    results_and_choices <- handle_calculate_results(bone_data)
  }, error = function(e) {
    stop("Error calculating results. Stopping.", e)
  })
  
  tryCatch({
    save_results_choices_inconclusives(results_and_choices$results, results_and_choices$choices, bone_data)
  }, error = function(e) {
    stop("Error saving results. Stopping.", e)
  })
}

handle_data_fetch <- function(bone_data) {
  if (missing(bone_data) # if they did not provide data
      || (!missing(bone_data) # of if the data they provide is bad
          && ((length(bone_data) == 0)
              || is.null(bone_data)
              || sum(is.na(bone_data)) == length(bone_data)))) {
    bone_data <- data.generator()
  }
  return(bone_data)
}

# calculates results and choices for one bone
handle_calculate_results <- function(bone_data) {
  choices <- tibble()
  results <- tibble()
  
  for (bone in bone_data) {
    res <- NULL
    tryCatch({
      res <- calculate_result_and_choose(bone)
    }, error = function() {
      next
    })
    
    if (is.null(res)) {
      next
    }
    
    choices <- bind_rows(choices, res$choice)
    results <- bind_rows(results, res$result)
  }
  return(list(
    results = results, 
    choices = choices
  ))
  
}

# calculates young's modulus and chooses best estimate.
calculate_result_and_choose <- function(bone) {
  result <- ym.calculateAndDetermine(bone)
  if (is.null(result$results) || is.null(result$choice)) {
    return(NULL)
  }
  
  return(list(result = extract_result(result$results), choice = extract_choice(result$choice)))
}

# returns relevant values from young's modulus determination result
extract_choice <- function(choice) {
  return(tibble(
    name  = choice$name[[1]],
    method  = choice$method[[1]],
    slope  = choice$slope[[1]],
    score  = choice$score[[1]],
    strain  = choice$strain[[1]],
    inconclusive  = choice$inconclusive
  ))
}

# returns relevant values from young's modulus result
extract_result <- function(result) {
  return(tibble(
    name = result$name, 
    rsquared = result$r.squared, 
    max.slope = result$max.slope, 
    max.strain = result$max.strain, 
    max.score = result$max.score, 
    inflection.slope = result$inflection.slope, 
    inflection.strain = result$inflection.strain, 
    inflection.score = result$inflection.score, 
    fds.slope = result$fds.slope, 
    fds.strain = result$fds.strain, 
    fds.score = result$fds.score
  ))
}

# creates a directory `path` if it does not exist
create_dir_safe <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path)
  }
}

# save the results, choices, inconclusives, and images.
save_results_choices_inconclusives <- function(results, choices, data) {
  current_date <<- Sys.Date() # set global date for use in other places in this file.
  
  output_dir <- file.path("results", "youngs-modulus", as.character(current_date))
  create_dir_safe(output_dir)
  
  inconclusive_data_dir <- file.path(output_dir, "inconclusive-data")
  create_dir_safe(inconclusive_data_dir)

  tryCatch({
    save_results(results, output_dir)
    save_choices(choices, output_dir)
    save_inconclusives(
      choices %>%
        dplyr::filter(inconclusive) %>%
        dplyr::pull(name),
      output_dir,
      inconclusive_data_dir
    )
    save_images(results, choices, data)
    
    message("\033[32mResults saved to to ", output_dir, "\033[0m")
  }, error = function(e) {
    message("Error saving automated young's modulus results")
  })
}

save_results <- function(results, output_results_dir) {
  write_csv(results, file.path(output_results_dir, "results.csv"))
}

save_choices <- function(choices, output_choices_dir) {
  write_csv(choices, file.path(output_choices_dir, "choices.csv"))
}
# saves inconclusive names to text file and copies original data to inconclusive folder
save_inconclusives <- function(inconclusive_names, output_dir, output_inconclusives_dir) {
  if (!length(inconclusive_names)) { # ha, as if
    message("No inconclusive choices! Yippeee!!")
    return()
  }
  
  copy_inconclusives(inconclusive_names, output_inconclusives_dir)
  
  inconclusive_txtfile_text <- c(paste("Generated on:", Sys.time()), inconclusive_names)
  inconclusives_txt_path <- file.path(output_dir, "inconclusives.txt")
  writeLines(inconclusive_txtfile_text, inconclusives_txt_path)
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

# saves images of data with young's modulus values and locations indicated
save_images <- function(results, choices, data) {
  
}



