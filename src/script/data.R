library(coro)
library(furrr)
library(progressr)

cleanArea <- function(df) {
  return (
    df |> 
      dplyr::rename(Segment = "Segment (UT, MT, LT or CP)",
                    Trial = "Trial # (at least 01-03)",
                    Area = "Area (m^2)",
                    Length = "Length (mm)",
                    Width = "Width or Diameter (mm)")
  )
}

global.area <- cleanArea(suppressMessages(read_csv("data/area.csv")))
global.load_filter <- 0.8


# Get your bones here! data.fetch fetches any number of fish, segments, and trials.
# arg fish_numbers: a list of fish numbers 1-21. default is all (1-21)
# arg segments: a list of segments "cp", "lt", "mt", or "ut". default is all
# arg trials: a list of trials. default is 1
# OR arg subject.name: your subject like "<fish type><fish number><segment><trial>" (without the < >)
data.fetch <- function(fish.type = "pf", fish_numbers = c(1:21), segments = c("cp", "lt", "mt", "ut"), trials = c(1), subject.name) {
  source("./src/script/helpers/general.R") # gen.parseSubjectName()
  
  # they just want one
  if (!missing(subject.name)) {
    return(findOne(parseSubjectName(subject.name)))
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
        results[[i]] <- bones[[trial]]
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


batchProcessFiles <- function(filepaths) {
  message("Fetching data...")
  with_progress({ # a progress bar!
    progress.bar <- progressor(steps = length(filepaths))
    lapply(filepaths, function(filepath) {
      data <- readAndProcessFile(filepath)
      progress.bar()
      return(data)
    })
  })
}

data.generator <- function(data.dir = "./data", fish.type, fish.number, segment) {
  filepaths <- getBoneFilepaths(data.dir, fish.type, fish.number, segment)
  #data <- batchProcessFilesPar(filepaths)
  data <- batchProcessFiles(filepaths)
  if (length(data) == 0) stop(paste("No data found for fish", fish.number))
  return(data)
}


readAndProcessFile <- function(filepath) {
  metadata <- parseFileName(filepath)
  data <- file.read(filepath)
  
  if (is.null(data)) {
    return(NULL)
  }
  
  processed_data <- recalculate(data, global.load_filter, metadata, global.area)
  attachMetadata(processed_data, metadata)
}

getBoneFilepaths <- function(data.dir = "./data", fish.type, fish.number, segment, trial) {
  path <- data.dir
  pattern <- "[^area].csv"
  if (!missing(fish.number)) {
    if (!missing(segment)) {
      pattern <- paste0(tolower(segment), "[0-9]{2}")
          
    }
      folder <- paste0(fish.type, str_pad(fish.number, 2, side = "left", pad = "0"))
      # get only one bone
      if (!missing(trial)) {
        path <- paste0(data.dir, "/", folder, "/")
        pattern <- paste0(folder, segment, str_pad(trial, 2, side = "left", pad = "0"), ".csv")
        return(unlist(list.files(path = path, pattern = pattern, recursive = FALSE, full.names = TRUE)))
      }
  }
  
  # gets all in data folder
  return(list.files(path = path, pattern = pattern, recursive = TRUE, full.names = TRUE))
}

parseFileName <- function(filepath) {
  file.name <- sub(".*/", "", filepath)
  individual <- str_sub(file.name, 1, 4)
  segment <- toupper(str_sub(file.name, 5, 6))
  trial <- parse_number(str_sub(file.name, 7, 8))
  return(c(individual, segment, trial))
}

# files are either comma or tab separated. This is indicated by the presence of "sep=\t" on the first line of the file. 
# If this line is there, the file is tab separated. If it is not, the file is comma separated.
# Returns correct file delimiter character
getDelim <- function(lines) {
  return(ifelse(grepl("sep=", lines[[1]]), "\t", ","))
}

# Returns correct number of lines to skip. Logic to find this is to skip up to where the line starts with "Reading".
getNumRowSkip <- function(lines) {
  return(grep("^Reading", lines)[1] - 1)
}

file.read <- function(filepath) {
  lines <- readLines(filepath, n = 20)
  delim <- getDelim(lines)
  num.skip <- getNumRowSkip(lines)
  
  df <- suppressWarnings(suppressMessages(read_delim(filepath, skip = num.skip, delim = delim)))
  
  if (!is.null(df)) {
    return(clean_fish_data(df))
  }
  return(NULL)
}

# wrapper around recalculate distance and recalculate stress strain
recalculate <- function(df, load_filter, metadata, area_data) {
  return(
    recalculateDistance(df, load_filter) |>
      recalculateStressStrain(metadata, area_data)
  )
}

recalculateDistance <- function(df, loadFilter) {
  return(
    df |> 
      filter(Load > loadFilter) |> 
      mutate(Distance = Distance - first(Distance)) # should new distance be negative?
  )
}

recalculateStressStrain <- function(df, metadata, area_data) {
  new_values <- getAreaAndInitialLength(metadata, area_data)
  area <- new_values[1]$Area
  length_initial <- new_values[2]$Length
  return(df |> 
           mutate(
             Stress = (Load / area)/10^6,
             Strain = abs(length_initial - (length_initial - Distance)) / length_initial))

}

getAreaAndInitialLength <- function(metadata, area_data) {
  return (
    area_data |> 
      filter(Individual == metadata[1], Segment == metadata[2], Trial == metadata[3]) |> 
      summarise(
        Area = first(Area),
        Length = first(Length)
      )
  )
}

attachMetadata <- function(df, metadata) {
  return(df |> mutate(Individual = metadata[1], Segment = metadata[2], Trial = metadata[3]))
}


clean_fish_data <- function(df) {
  return(df |> 
           dplyr::rename(Load = "Load [N]",
                  Time = "Time [s]",
                  Distance = "Distance [mm]") |> 
           dplyr::select(-where(is.logical))) # if the last column is blank (it sometimes or always is), remove it
}


data_clean <- function(data_name, area, length_initial, load) {
  
  data_string <- deparse(substitute(data_name))
  
  data <- get(data_string)
  
  processed_data <- data |>
    dplyr::select(-7) |>
    rename(Load = "Load [N]",
           Time = "Time [s]",
           Stress = "Stress [MPa]",
           Strain = "Strain [%]",
           Distance = "Distance [mm]") |>
    filter(Load > load) |>
    mutate(Area = area,
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



