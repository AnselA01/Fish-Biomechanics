data.generator <- generator(function(data_dir, fish_number, segment) {
  filepath_list <- get_fish_data_file_names(data_dir, fish_number)
  if (!length(filepath_list)) {
    stop(paste("No data found for fish", fish_number))
  }
  
  for (file_path in filepath_list) {
    metadata <- parse_file_name(file_path)
    data <- read_file(file_path)
    if (is.null(data)) {
      next
    }
    
    area_data <- clean_area_data(suppressMessages(read_csv("data/area.csv")))
    load_filter <- 0.8
    
    yield(
      recalculate(data, load_filter, metadata, area_data) %>% 
        attach_metadata(metadata)
    )
  }
})

get_fish_data_file_names <- function(data_dir, fish_number, segment) {
  path <- data_dir
  pattern <- "[^area].csv"
  if (!missing(fish_number)) {
    if (!missing(segment)) {
      pattern <- paste0("[^area]", segment, "[0-9]{2}")
          
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

read_file <- function(file_path) {
  rows_skip <- 15
  df <- suppressMessages(read_tsv(file_path, skip = rows_skip))
  if (ncol(df) == 7) {
    return(clean_fish_data(df))
  }
}

recalculate <- function(df, load_filter, metadata, area_data) {
  return(
    recalculate_distance(df, load_filter) %>% 
           recalculate_stress_strain(metadata, area_data))
}

recalculate_distance <- function(df, loadFilter) {
  return(
    df %>% 
      filter(Load > loadFilter) %>% 
      mutate(Distance = Distance - first(Distance)) # should new distance be negative?
  )
}

recalculate_stress_strain <- function(df, metadata, area_data) {
  new_values <- find_area_initial_length(metadata, area_data)
  area <- new_values[1]$Area
  length_initial <- new_values[2]$Length
  return(df %>% 
           mutate(
             Stress = (Load / area)/10^6,
             Strain = abs(length_initial - (length_initial - Distance)) / length_initial))
}

find_area_initial_length <- function(metadata, area_data) {
  return (
    area_data %>% 
      filter(Individual == metadata[1], Segment == metadata[2], Trial == metadata[3]) %>% 
      summarise(
        Area = first(Area),
        Length = first(Length)
      )
  )
}

attach_metadata <- function(df, metadata) {
  return(df %>% mutate(Individual = metadata[1], Segment = metadata[2], Trial = metadata[3]))
}

clean_area_data <- function(df) {
  return (
    df %>% 
      dplyr::rename(Segment = "Segment (UT, MT, LT or CP)",
             Trial = "Trial # (at least 01-03)",
             Area = "Area (m^2)",
             Length = "Length (mm)",
             Width = "Width or Diameter (mm)")
  )
}

clean_fish_data <- function(df) {
  return(df %>%
           dplyr::rename(Load = "Load [N]",
                  Time = "Time [s]",
                  Stress = "Stress [MPa]",
                  Strain = "Strain [%]",
                  Distance = "Distance [mm]") %>% 
           dplyr::select(-7))
}


data_clean <- function(data_name, area, length_initial, load) {
  
  data_string <- deparse(substitute(data_name))
  
  data <- get(data_string)
  
  processed_data <- data %>%
    dplyr::select(-7) %>%
    rename(Load = "Load [N]",
           Time = "Time [s]",
           Stress = "Stress [MPa]",
           Strain = "Strain [%]",
           Distance = "Distance [mm]") %>%
    filter(Load > load) %>%
    mutate(Area = area,
           Distance = Distance - first(Distance),
           LengthInitial = length_initial,
           Stress = 10^-6 * Load / Area,
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



