library(coro)
library(furrr)
library(progressr)
library(plyr)
library(dplyr)
library(stringr)

cleanArea <- function(area_df) {
  # first check if they have provided the good names
  if (all(c("Segment", "Trial", "Area", "Length", "Width") %in% names(area_df))) return(area_df)
  return (
    area_df |>
      dplyr::rename(
        Segment = "Segment (UT, MT, LT or CP)",
        Trial = "Trial # (at least 01-03)",
        Area = "Area (m^2)",
        Length = "Length (mm)",
        Width = "Width or Diameter (mm)"
      )
  )
}

global.area <- cleanArea(suppressMessages(read_csv("data/area.csv"))) # this must be here for everything to work
global.load_filter <- 0.8 # see src/md/youngsModulus.../thresholdjustification.Rmd


# data.fetch fetches any number of fish, segments, and trials.
# arg fish_numbers: a list of fish numbers 1-21. default is all (1-21)
# arg segments: a list of segments "cp", "lt", "mt", or "ut". default is all
# arg trials: a list of trials. default is 1
# OR arg subject.name: your subject like "<fish type><fish number><segment><trial>" (without the < >)
data.fetch <- function(fish.type = "pf", fish_numbers = c(1:21), segments = c("cp", "lt", "mt", "ut"), trials = c(1), subject.name) {
  source("./src/script/helpers/general.R") # gen.parseSubjectName()

  # they just want one
  if (!missing(subject.name)) {
    return(findOne(parseSubjectName(tolower(subject.name))))
  }

  results.max <- length(fish_numbers) * length(segments) * length(trials)
  results <- vector("list", results.max)
  names <- vector("character", results.max)
  i <- 1
  
  for (fish_number in fish_numbers) {
    for (segment in segments) {
        bones <- data.generator(fish.type, fish_number = fish_number, segment = segment)
        if (!length(bones)) next
        # they can't have more trials than there are available.
        segment_trials <- if (length(trials) > length(bones)) 1:length(bones) else trials
      for (trial in segment_trials) {
        bone <- bones[[trial]]
        results[[i]] <- bone
        names[[i]] <- paste0(sprintf("%02d", fish_number), segment, trial)      
      }
    }
  }
  
  results <- results[seq_len(i - 1)]
  names(results) <- names[seq_len(i - 1)]
  if (length(results) == 1) return(results[[1]]) # return just the one 
  return(results)
}

# finds one bone given the arguments. Can be used on its own but is more easily interfaced with data.fetch
findOne <- function(attributes) {
  filepath <- getBoneFilepaths("./data", attributes$fish.type, attributes$fish.number, attributes$segment, attributes$trial)
  if (!length(filepath)) {
    stop(paste("No data found for fish", attributes$fish.number))
  }
  return(readAndProcessFile(filepath))
}

handlers("progress", append = TRUE)
batchProcessFiles <- function(filepaths) {
  message("\033[37mFetching data...\033[0m")
  with_progress({
    progress.bar <- progressor(steps = length(filepaths))
    
    lapply(filepaths, function(filepath) {
      data <- readAndProcessFile(filepath)
      if (is.null(data)) {
        progress.bar()
        return(NULL)
      }
      progress.bar()
      data
    }) %>% compact()
  })
}

data.generator <- function(data.dir = "./data", fish.type = NULL, fish.number = NULL, segment = NULL, trial = NULL) {
  filepaths <- getBoneFilepaths(data.dir, fish.type, fish.number, segment, trial)
  data <- batchProcessFiles(filepaths)
  if (length(data) == 0) stop("No data found")
  
  message(paste("\033[37mFound", length(data), "observations\033[0m"))
  
  return(data)
}

readAndProcessFile <- function(filepath) {
  identifying_lines <- readLines(filepath, n = 30)
  if (length(identifying_lines) == 0) {
    return(NULL)
  }
  data <- file.read(filepath, identifying_lines)
  # the second row of the data is its identifier
  metadata <- extract_metadata(identifying_lines[2])
  # this means that the bone identifer is not in the file contents. 
  # The backup plan is to parse it from the filename. We prefer not to assume the filename is correct.
  if (is.na(metadata$individual)) {
    metadata <- extract_metadata(basename(filepath))
  }

  if (is.null(data)) {
    return(NULL)
  }
  
  processed_data <- recalculate(data, global.load_filter, metadata)
  if (is.null(processed_data)) {
    return(NULL)
  }
  return(attachMetadata(processed_data, metadata))
}

pattern <- "[a-zA-Z]{2}\\d+[a-zA-Z]{2}\\d+\\.csv$"
getBoneFilepaths <- function(data.dir = file.path("data"), fish.type, fish.number, segment, trial) {
  path <- data.dir
  # matches the bone file naming scheme like pf01cp01 or pf100cp100
  if (!is.null(fish.number)) {
    if (!is.null(segment)) {
      pattern <- paste0(tolower(segment), "[0-9]+")
  
    }
      folder <- paste0(fish.type, str_pad(fish.number, 2, side = "left", pad = "0"))
      # get only one bone
      if (!is.null(trial)) {
        path <- paste0(data.dir, "/", folder, "/")
        pattern <- paste0(folder, segment, str_pad(trial, 2, side = "left", pad = "0"), ".csv")
        return(unlist(list.files(path = path, pattern = pattern, recursive = FALSE, full.names = TRUE)))
      }
  }
  
  # they didn't specify segment or trial number, give them everything!
  return(list.files(path = path, pattern = pattern, recursive = TRUE, full.names = TRUE))
}

parseFileName <- function(filename) {
  identifiers <- str_match(filename, "^([a-zA-Z]{2})(\\d+)([a-zA-Z]{2})(\\d+)\\.csv$")

  if (is.na(identifiers[1, 1])) {
    return(NULL)
  }

  individual <- identifiers[1,2]
  segment <- toupper(identifiers[1, 4])
  trial <- as.integer(identifiers[1, 5])

  return(c(individual, segment, trial))
}


# dynamically grabs correct file delimiter. Grabs all text after "sep=". Will either be a comma "," or a tab "\t".

getDelim <- function(identifying_lines) {
  delim <- str_extract(identifying_lines[[1]], "(?<=sep=).*")
  no_delim <- is.na(delim) | delim == "" # condition for bone file has no delim.
  
  if (any(no_delim)) { # uh oh no delim. Parse it from the text separating "Reading" and "Load". 
    delim <- str_extract(identifying_lines, "(?<=Reading)(.*?)(?=Load)") %>% 
      na.omit() %>% 
      as.vector()
  } 
  
  if (any(no_delim)) {
    return(NULL)
  }
  return(delim)
}

# Returns correct number of lines to skip. Logic to find this is to skip up to where the line starts with "Reading".
getNumRowSkip <- function(identifying_lines) {
  return(grep("^Reading", identifying_lines)[1] - 1)
}

file.read <- function(filepath, identifying_lines) {
  delim <- getDelim(identifying_lines)
  if (is.null(delim)) {
    return(NULL)
  }
  num.skip <- getNumRowSkip(identifying_lines)
  
  df <- suppressWarnings(suppressMessages(read_delim(filepath, skip = num.skip, delim = delim)))
  
  if (!length(names(df))) {
    return(NULL)
  }

  if (!is.null(df)) {
    return(clean_fish_data(df))
  }
  return(NULL)
}

# wrapper around recalculate distance and recalculate stress strain
recalculate <- function(df, load_filter, metadata) {
  return(
    recalculateDistance(df, load_filter) %>% 
      recalculateStressStrain(metadata)
  )
}

recalculateDistance <- function(df, loadFilter) {
  return(
    df %>% 
      dplyr::filter(Load > loadFilter) |> 
      dplyr::mutate(Distance = Distance - first(Distance)) # should new distance be negative?
  )
}

recalculateStressStrain <- function(df, metadata) {
  new_values <- getAreaAndInitialLength(metadata)
  if (is.null(new_values)) {
    return(NULL)
  }
  area <- new_values[1]$Area
  length_initial <- new_values[2]$Length
  return(df |> 
           dplyr::mutate(
             Stress = (Load / area)/10^6,
             Strain = abs(length_initial - (length_initial - Distance)) / length_initial))

}

getAreaAndInitialLength <- function(metadata) {
  if(!bone_is_in_area_data(metadata, global.area[, 3:5])) {
    message(paste0(metadata), " not found in area data")
    return (NULL)
  }
  
  return (
    global.area |> 
      dplyr::filter(Individual == metadata[1], Segment == metadata[2], Trial == metadata[3]) |> 
      summarise(
        Area = first(Area),
        Length = first(Length)
      )
  )
}

attachMetadata <- function(df, metadata) {
  return(df |> dplyr::mutate(Individual = metadata$individual, Segment = metadata$segment, Trial = metadata$trial))
}

clean_fish_data <- function(df) {
  return(df |> 
           dplyr::rename(Load = "Load [N]",
                  Time = "Time [s]",
                  Distance = "Distance [mm]") |> 
           dplyr::select(-where(is.logical))) # if the last column is blank (it sometimes is), remove it
}

data_clean <- function(data_name, area, length_initial, load) {
  
  data_string <- deparse(substitute(data_name))
  
  data <- get(data_string)
  
  processed_data <- data |>
    dplyr::select(-7) |>
    dplyr::rename(Load = "Load [N]",
           Time = "Time [s]",
           Stress = "Stress [MPa]",
           Strain = "Strain [%]",
           Distance = "Distance [mm]") |>
    dplyr::filter(Load > load) |>
    dplyr::mutate(Area = area,
           Distance = Distance - first(Distance),
           LengthInitial = length_initial,
           Stress = 10^-6 * (Load / Area),
           Strain = abs((length_initial - (length_initial - Distance)) / length_initial),
           Fish_Type = substr(data_string, 0, 2),
           Fish_Num = substr(data_string, 3, 4),
           Bone_Type = substr(data_string, 5 ,6),
           Bone_Num = substr(data_string, 7 ,8)) 
  
  assign(data_string, processed_data, envir = .GlobalEnv)
}

plot <- function(data) {
  return(ggplot(data, aes(x = Strain, y = Stress)) +
           geom_point() +
           labs(caption = paste("Fish number: ", data$Individual, ", Segment: ",  data$Segment, ", Trial Number: ", data$Trial_Number, sep = ""),
                x = "Strain",
                y = "Stress") +
           theme_minimal()) +
    plot.caption = element_text(hjust = 0, size = 10)
}



