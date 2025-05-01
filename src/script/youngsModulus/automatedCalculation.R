# A wrapper around youngsModulus.R and youngsModulusDetermination.R to 
# perform Young's Modulus calculation and decision making for all data in
# the "data" directory as well as install and load all required packages and sources

required_packages <- c(
  "here",
  "readr",
  "tibble",
  "coro",
  "furrr",
  "progressr",
  "plyr",
  "dplyr",
  "stringr",
  "stringi",
  "ggplot2",
  "gridExtra",
  "ggtext",
  "splines2",
  "cowplot"
)

required_sources <- c(
  "src/script/helpers/data.R",
  "src/script/helpers/general.R",
  "src/script/helpers/image.R",
  "src/script/helpers/plot.R",
  "src/script/youngsModulus/youngsModulus.R",
  "src/script/youngsModulus/youngsModulusDetermination.R"
)

current_date <- NULL

# install if necessary and load all required packages and source our R files
load_libraries_and_source <- function() {
  all_installed_flag <- TRUE
  need_install <- c()
  
  for (package in required_packages) {
    if (!requireNamespace(package, quietly = TRUE)) { 
      all_installed_flag <- FALSE
      need_install <- append(need_install, package)
      next
    }
    
    successful_load <- library(package, 
                       character.only = TRUE, 
                       logical.return = TRUE)
    
    if (!successful_load) {
      quit("Error loading package", package, "Exiting.")
    }
  }
  
  if (!all_installed_flag) {
    message("Installing required packages. This will happen only once.")
    Sys.sleep(2) # give time to read the message
  }
  
  # install uninstalled packages
  for (needs_install in need_install) {
    # library() returns boolean indicating whether package is installed
    successful_install <- library(needs_install, 
                                  character.only = TRUE,
                                  logical.return = TRUE)
    if (!successful_install) {
      quit("Error installing package ", needs_install, " Exiting.")
    }
  }
  
  setwd(here::here()) # here() returns the "reasonable" (based on a heuristic) project root.

  # load our R sources
  lapply(required_sources, function(source) source(here::here(getwd(), source)))
}

# Entry point for automated Young's modulus calculation. Handles data fetching, results calculation, and saving.
# * arg bone_data is optional.
entry <- function(bone_data = NA) {
  load_libraries_and_source()
  
  current_date <<- Sys.Date() # set global date. format: YYYY-MM-DD
  
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
    handle_save_results_choices_inconclusives(results_and_choices$data, results_and_choices$results, results_and_choices$choices)
  }, error = function(e) {
    stop("Error saving results. Stopping.", e)
  })
}

handle_data_fetch <- function(bone_data) {
  if (is.na(bone_data) # if they did not provide data
      || (is.null(bone_data) # of if the data they provide is bad
          && ((length(bone_data) == 0)
              || sum(is.na(bone_data)) == length(bone_data)))) {
    bone_data <- data.generator()
  }
  return(bone_data)
}

# calculates results and choices for one bone
handle_calculate_results <- function(bone_data) {
  choices <- tibble()
  results <- tibble()

  message("\033[37mCalculating Young's modulus...\033[0m")

  with_progress({
    progress.bar <- progressor(along = bone_data)
    for (bone in bone_data) {
      res <- tryCatch({
        calculate_result_and_choose(bone)
      }, error = function(e) {
        NULL
      })
      
      if (is.null(res)) {
        progress.bar()
        next
      }
      
      choices <- bind_rows(choices, res$choice)
      results <- bind_rows(results, res$result)
      progress.bar()
    }
  })

  data_filtered <- Filter(function(bone) {
    getName(bone) %in% results$name
  }, bone_data)
  
  return(list(
    data = data_filtered,
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
handle_save_results_choices_inconclusives <- function(data, results, choices) {
  output_dir <- file.path("results", "youngs-modulus", as.character(current_date))
  create_dir_safe(output_dir)
  
  inconclusive_data_dir <- file.path(output_dir, "inconclusive-data")
  create_dir_safe(inconclusive_data_dir)
  
  images_dir <- file.path(output_dir, "images")
  create_dir_safe(images_dir)

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
    save_images(data, results, choices, images_dir)
    
    message("\033[32mResults saved to ", output_dir, "\033[0m")
  }, error = function(e) {
    message("Error saving automated young's modulus results\n", e)
  })
}

save_results <- function(results, output_results_dir) {
  write_csv(results, here::here(output_results_dir, "results.csv"))
}

save_choices <- function(choices, output_choices_dir) {
  write_csv(choices, here::here(output_choices_dir, "choices.csv"))
}
# saves inconclusive names to text file and copies original data to inconclusive folder
save_inconclusives <- function(inconclusive_names, output_dir, output_inconclusives_dir) {
  if (!length(inconclusive_names)) { # ha, as if
    message("No inconclusive choices! Yippeee!!")
    return()
  }
  
  copy_inconclusives(inconclusive_names, output_inconclusives_dir)
  
  inconclusive_txtfile_text <- c(paste("Generated on:", Sys.time()), inconclusive_names)
  inconclusives_txt_path <- here(output_dir, "inconclusives.txt")
  writeLines(character(0), inconclusives_txt_path) # empty the file
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
    raw_data_path <- here("data", dirname, filename)
    
    file.copy(raw_data_path, inconclusive_data_dir)
  }
}

# saves images of data with young's modulus values and locations indicated
save_images <- function(data, results, choices, images_dir) {
  
  numBones <- length(data)
  
  message("\033[37mSaving images...\033[0m")
  with_progress({
    progress.bar <- progressor(steps = numBones)
    for (i in 1:numBones) {
      plot <- plot.youngsModulusDecision(data[[i]], results[i, ], choices[i, ])
      
      name <- tolower(getName(data[[i]]))
      matches <- str_match(name, "^([a-z]{2,})([0-9]{2,})") # first two+ lowercase letters and first two+ numbers for the folder name
      individual <- paste0(matches[,2], matches[,3])
      
      individual_dir <- here(images_dir, individual)
      create_dir_safe(individual_dir)
      image_path <- file.path(individual_dir, paste0(name, ".jpg"))
      image.save2(plot, image_path)
      progress.bar()
    }
  })
  
  cat("\n")
}



