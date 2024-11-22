library(coro)



# Get your bones here! data.fetch fetches any number of fish, segments, and trials.
# arg fish_numbers: a list of fish numbers 1-21. default is all (1-21)
# arg segments: a list of segments "cp", "lt", "mt", or "ut". default is all
# arg trials: a list of trials. default is 1
data.fetch <- function(fish_numbers = c(1:21), segments = c("cp", "lt", "mt", "ut"), trials = c(1)) {
  results <- list()
  names <- list()
  for (fish_number in fish_numbers) {
    for (segment in segments) {
        bones <- collect(data.generator(fish_number = fish_number, segment = segment))
        if (!length(bones)) next
        # they can't have more trials than there are available.
        segment_trials <- if (length(trials) > length(bones)) 1:length(bones) else trials
      for (trial in segment_trials) {
        results <- append(results, list(bones[[trial]]))
        names <- append(names, paste0(sprintf("%02d", fish_number), segment, trial))
      }
    }
  }
  
  if (length(results) == 1) return(results[[1]]) # if there is only one result, unlist it
  names(results) <- names
  return(results)
}

data.generator <- generator(function(data_dir = "./data", fish_number, segment) {
  filepath_list <- get_fish_data_file_names(data_dir, fish_number, segment)
  if (!length(filepath_list)) {
    stop(paste("No data found for fish", fish_number))
  }
  
  for (file_path in filepath_list) {
    metadata <- parse_file_name(file_path)
    data <- read_file(file_path)
    if (is.null(data)) {
      next
    }
    
    area <- area.clean(suppressMessages(read_csv("data/area.csv")))
    load_filter <- 0.8
    
    yield(
      recalculate(data, load_filter, metadata, area) |> 
        attach_metadata(metadata)
    )
  }
})

get_fish_data_file_names <- function(data_dir, fish_number, segment) {
  path <- data_dir
  pattern <- "[^area].csv"
  if (!missing(fish_number)) {
    if (!missing(segment)) {
      pattern <- paste0(tolower(segment), "[0-9]{2}")
          
    }
      folder <- paste0("pf", str_pad(fish_number, 2, side = "left", pad = "0"))
      path <- paste(data_dir, folder, sep = "/")
  }
  return(list.files(path = path, include.dirs = TRUE, recursive = TRUE, full.names = TRUE, pattern = pattern))
}

parse_file_name <- function(full_file_path) {
  file_name <- sub(".*/", "", full_file_path)
  individual <- str_sub(file_name, 1, 4)
  segment <- toupper(str_sub(file_name, 5, 6))
  trial <- parse_number(str_sub(file_name, 7, 8))
  return(c(individual, segment, trial))
}

# files are either comma or tab separated. This is indicated by the presence of "sep=\t" on the first line of the file. 
# If this line is there, the file is tab separated. If it is not, the file is comma separated.
# Returns correct file delimiter character
getDelim <- function(file_path) {
  first.line <- readLines(file_path, n = 1)
  return(ifelse(grepl("sep=", first.line), "\t", ","))
}

# Returns correct number of lines to skip. Logic to find this is to skip up to where the line starts with "Reading".
getRowSkip <- function(file_path) {
  return(grep(paste0("^", "Reading"),  readLines(file_path)) - 1)
}

read_file <- function(file_path) {
  row.skip <- getRowSkip(file_path)
  delim <- getDelim(file_path)
  df <- suppressMessages(read_delim(file_path, skip = row.skip, delim = delim))
  
  if (!is.null(df)) {
    return(clean_fish_data(df))
  }
  return (NULL)
}

# wrapper around recalculate distance and recalculate stress strain
recalculate <- function(df, load_filter, metadata, area_data) {
  return(
    recalculate_distance(df, load_filter) |> 
           recalculate_stress_strain(metadata, area_data))
}

recalculate_distance <- function(df, loadFilter) {
  return(
    df |> 
      filter(Load > loadFilter) |> 
      mutate(Distance = Distance - first(Distance)) # should new distance be negative?
  )
}

recalculate_stress_strain <- function(df, metadata, area_data) {
  new_values <- find_area_initial_length(metadata, area_data)
  area <- new_values[1]$Area
  length_initial <- new_values[2]$Length
  return(df |> 
           mutate(
             Stress = (Load / area)/10^6,
             Strain = abs(length_initial - (length_initial - Distance)) / length_initial))

}

find_area_initial_length <- function(metadata, area_data) {
  return (
    area_data |> 
      filter(Individual == metadata[1], Segment == metadata[2], Trial == metadata[3]) |> 
      summarise(
        Area = first(Area),
        Length = first(Length)
      )
  )
}

attach_metadata <- function(df, metadata) {
  return(df |> mutate(Individual = metadata[1], Segment = metadata[2], Trial = metadata[3]))
}

area.clean <- function(df) {
  return (
    df |> 
      dplyr::rename(Segment = "Segment (UT, MT, LT or CP)",
             Trial = "Trial # (at least 01-03)",
             Area = "Area (m^2)",
             Length = "Length (mm)",
             Width = "Width or Diameter (mm)")
  )
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
           geom_point() +        # Points for the data
           labs(caption = paste("Fish number: ", data$Individual, ", Segment: ",  data$Segment, ", Trial Number: ", data$Trial_Number, sep = ""),
                x = "Strain",
                y = "Stress") +
           theme_minimal()) +
    plot.caption = element_text(hjust = 0, size = 10)
}



