# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Jon Harcum, 2024-04-11
#' @title Initialize Modeling Configuration Options
#'
#' @description Initializes modeling configuration variables from a JSON
#' configuration file. This includes setting up various parameters such as
#' waterbody identifiers (subestuary), Generalized Additive Model (GAM) formulas
#' , temporal domains, and data sources. It's designed to facilitate the setup
#' process for environmental modeling by organizing and validating configuration
#'  settings required for model execution.
#'
#' @details The function reads and validates the JSON configuration file,
#' extracting key modeling parameters and initializing them for use in the
#' modeling process. This includes the configuration for GAM formulas, date
#' ranges, data sources, water quality variables, transformation names, and
#' parameters. It also allows for the selection of subestuary domains based on
#' the provided subestuary data frame. Optional verbose output provides a
#' detailed overview of the initialized settings for verification and review.
#'
#' @param config_file A string specifying the path to the JSON configuration
#' file containing modeling settings.
#' @param subestuary A data frame containing information about subestuaries,
#' which is required for initializing certain model configuration lists like
#' `cfg_estuary_lst` and `cfg_by_term`.
#' @param show_msgs Logical; if `TRUE`, prints detailed information about the
#' initialized modeling configuration to the console.
#'
#' @return A list containing the initialized configuration variables for
#' modeling. This includes selections for subestuary domains, water quality
#' variables, GAM formulas, transformation parameters, and more.
#'
#' @examples
#' subestuary_data <- data.frame(cb4d_subestuary = c("CB1"
#'                                                   , "CB2"
#'                                                   , "CB3"
#'                                                   , "CB4"
#'                                                   , "CB5"))
#' modeling_vars <- init_model_config("path/to/config_file.json"
#'                                    , subestuary_data)
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr distinct pull mutate rowwise ungroup
#' @importFrom lubridate with_tz
#' @importFrom magrittr %>%
#' @importFrom flextable flextable compose autofit theme_box as_paragraph
#'
#' @export
init_model_config <- function(config_file, subestuary, show_msgs=FALSE) {

  # config_file <- "cb4d_data/config_file_model_settings.json"; subestuary= model_data[["subestuary"]]; show_msgs=TRUE

  message("Initializing model configuration variables ...")

  # Validate input
  if (!"data.frame" %in% class(subestuary)) {
    stop("subestuary must be a data frame.")
  }

  # Load configuration from JSON
  config <- fromJSON(config_file)

  # Initialize variables from configuration
  cfg_gam_formula <- config$cfg_gam_formula

  cfg_date_range <-
    as_tibble(config$cfg_date_range) %>%
    rowwise() %>%
    mutate(start = with_tz(start, tzone = time_zone)) %>%
    mutate(end =   with_tz(end, tzone = time_zone)) %>%
    ungroup()

  cfg_data_source <- config$cfg_data_source

  cfg_wq_var <- config$cfg_wq_var

  # Extract transformation names and parms
  cfg_trans_names <- config$cfg_transformations$trans_name
  cfg_trans_parms <- config$cfg_transformations$trans_parms

  # Extract model names and GAM specifications
  cfg_gam_formula_name <- cfg_gam_formula$formula_name
  cfg_gam_formula      <- cfg_gam_formula$formula

  # User selected spatial domain; set "by term" for GAM
  cfg_subestuary <- subestuary %>%
    distinct(cb4d_subestuary) %>%
    pull(cb4d_subestuary)

  # User selected spatial domain; set "by term" for GAM
  cfg_by_term <- names(subestuary)[grep("^by_", names(subestuary))]

  if (show_msgs) {

    # Function to format values with numerical order, each on a new line
    format_values <- function(values) {
      # Enumerate and separate values by newline
      enumerated_values <- paste(sapply(1:length(values)
                                        , function(i) paste(i
                                                            , ")"
                                                            ,  values[i]
                                                            , sep = ""))
                                 , collapse = "\n")
      return(enumerated_values)
    }

    cfg_date_range_fmt <- cfg_date_range %>%
      mutate(date_range = paste(format(start, "%Y-%m-%d %H:%M")
                                , "to"
                                , format(end, "%Y-%m-%d %H:%M"))) %>%
      pull(date_range)

    # Prepare data with enumerated values for each variable
    data <- list(
      cfg_subestuary = format_values(cfg_subestuary),
      cfg_wq_var = format_values(cfg_wq_var),
      cfg_trans_names = format_values(cfg_trans_names),
      cfg_gam_formula_name = format_values(cfg_gam_formula_name),
      cfg_by_term = format_values(cfg_by_term),
      cfg_date_lst = format_values(cfg_date_range_fmt),
      cfg_data_source = format_values(cfg_data_source)
    )

    # Convert to dataframe, adjusting for actual variable names
    df <- data.frame(
      Variable = c("cfg_subestuary"
                   , "cfg_wq_var"
                   , "cfg_trans_names"
                   , "cfg_gam_formula_name"
                   , "cfg_by_term"
                   , "cfg_date_lst"
                   , "cfg_data_source"),
      Settings = sapply(data, identity), # Extract the formatted values
      stringsAsFactors = FALSE
    )

    # Generate the flextable, ensuring line breaks are respected
    ft <- flextable(df) %>%
      flextable::compose(j = "Settings", value = as_paragraph(Settings)) %>%
      autofit()

    ft <- theme_box(ft)

    print(ft)
  }

  # Return a list of all initialized variables
  return(list(
    cfg_subestuary = cfg_subestuary,
    cfg_wq_var = cfg_wq_var,
    cfg_trans_names = cfg_trans_names,
    cfg_trans_parms = cfg_trans_parms,
    cfg_gam_formula_name = cfg_gam_formula_name,
    cfg_gam_formula = cfg_gam_formula,
    cfg_by_term = cfg_by_term,
    cfg_date_range = cfg_date_range,
    cfg_data_source = cfg_data_source
  ))

}## FUN ~ init_model_config

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Jon Harcum, 2024-04-11
#' @title Load and Prepare Input Data Files
#'
#' @description Loads and processes various datasets as specified in a JSON
#' configuration file. This function is designed to prepare the data for
#' modeling by loading from paths specified in the configuration file, selecting
#'  and mutating specific columns, and handling special cases such as negative
#'  values in `depth_b` within station data.
#'
#' @param config_file A string specifying the path to the JSON configuration
#' file, which outlines locations and specifications for input data files,
#' including water quality data, GIS segments, subestuary frameworks, and
#' station locations.
#' @param show_msgs Logical; if TRUE, prints messages during the data loading
#' and processing stages.
#' Defaults to FALSE.
#'
#' @return A list containing processed datasets: water quality data, GIS
#' segments, subestuary framework, and station locations, each ready for further
#'  analysis or modeling.
#'
#' @examples
#' \dontrun{
#' config_file_path <- "path/to/config_file_locations.json"
#' data_list <- load_and_prep_input_data(config_file_path, show_msgs = TRUE)
#' }
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr select mutate relocate case_when if_else
#' @importFrom readxl read_xlsx
#' @importFrom magrittr %>%
#'
#' @export
load_and_prep_input_data <- function(config_file, show_msgs=FALSE) {

  message("Loading and processing input data files ...")

  file_paths <- fromJSON(config_file)

  # Construct file paths using the JSON configuration
  file_wq_path         <- file.path(file_paths$water_quality_data$folder
                                    , file_paths$water_quality_data$data_file)
  file_seg_sf_path     <- file.path(file_paths$segment_sf$folder
                                    , file_paths$segment_sf$data_file)
  file_subestuary_path <- file.path(file_paths$subestuary_organization$folder
                                , file_paths$subestuary_organization$data_file)
  file_stations_path   <- file.path(file_paths$station_locations$folder
                                    , file_paths$station_locations$data_file)

  # Load and process water quality data set
  load(file_wq_path) -> loaded_var
  assign("data_wq", get(loaded_var))

  # Load and process GIS segments file
  load(file_seg_sf_path)
  data_seg_sf <- cb_cbseg92_sf %>%
    select(cbseg_92, cb_303d_segment, cbpseg, state, wq_segs, ow, dw, dc)

  # Load and process sub-estuary config
  data_subestuary <- read_xlsx(path = file_subestuary_path, na="NA") %>%
    select(cb4d_subestuary, cbseg_92, add_data, starts_with("by_")) %>%
    mutate(in_subestuary = !add_data) %>%
    relocate(in_subestuary, .before = add_data)

  # Load and process station locations, handling negative depth_b values
  data_stations <- read_xlsx(path = file_stations_path, na="NA") %>%
    mutate(cbseg_92 = case_when(is.na(cbseg_92) ~ cbseg_92_near,
                                TRUE ~ cbseg_92),
           depth_b = if_else(depth_b < 0.1, 0.1, depth_b)) %>%
    select(station
           , cbseg_92
           , latitude
           , longitude
           , depth_b
           , wb_lat_km
           , wb_lon_km
           , fixed )

  if (show_msgs) {
    print("... load_and_prep_input_data messages in development ..." )
  }

  return(list(data = data_wq,
              cbseg_92_sf = data_seg_sf,
              subestuary = data_subestuary,
              stations = data_stations,
              file_paths=file_paths))
}## FUN ~ load_and_prep_input_data


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Jon Harcum, 2024-04-11
#' @title Create Job Queue for Modeling
#'
#' @description Generates a comprehensive job queue based on various control
#' parameters and the model configuration. This function combines control
#' settings with the provided model configuration to form a detailed list of GAM
#' modeling jobs. Each job within the queue specifies the subsets of data and
#' configurations to be used in the modeling process, effectively organizing the
#'  modeling workflow.
#'
#' @param cntrl_subestuary Numeric or NA; control for selecting subestuaries for
#' modeling.
#' @param cntrl_gam_formula Numeric or NA; control for selecting GAM formulas.
#' @param cntrl_by_term Numeric or NA; control for selecting terms to model by.
#' @param cntrl_date Numeric or NA; control for selecting date ranges.
#' @param cntrl_data_source Numeric or NA; control for selecting data sources.
#' @param model_config A list containing the model configuration settings.
#' @param show_msgs Logical; if `TRUE`, displays messages regarding the job
#' queue creation process.
#'
#' @return A data frame representing the job queue, with each row specifying a
#' unique job configuration for modeling.
#'
#' @examples
#' \dontrun{
#' model_config <- init_model_config("path/to/config_file.json", subestuary_data)
#' job_queue <- create_job_que(model_config=model_config)
#' }
#'
#' @importFrom tidyr expand_grid
#' @importFrom dplyr mutate relocate ungroup
#' @importFrom magrittr %>%
#'
#' @export
create_job_que <- function(cntrl_subestuary = NA
                           , cntrl_wq_var = NA
                           , cntrl_trans_names = NA
                           , cntrl_gam_formula = NA
                           , cntrl_by_term = NA
                           , cntrl_date = NA
                           , cntrl_data_source = NA
                           , model_config = NA
                           , show_msgs = FALSE) {

  # Validate inputs
  if (!is.list(model_config)) {
    stop("model_config must be a list.")
  }

  validate_control_input <- function(input, name) {
    if ( any(is.na(input)) || any(!is.numeric(input)) || any(input < 1)) {
      stop(sprintf("%s must be NA or a positive integer.", name))
    }
  }

  validate_control_input(input=cntrl_subestuary, name="cntrl_subestuary")
  validate_control_input(cntrl_wq_var, "cntrl_wq_var")
  validate_control_input(cntrl_trans_names, "cntrl_trans_names")
  validate_control_input(cntrl_gam_formula, "cntrl_gam_formula")
  validate_control_input(cntrl_by_term, "cntrl_by_term")
  validate_control_input(cntrl_date, "cntrl_date")
  validate_control_input(cntrl_data_source, "cntrl_data_source")


  job_que <- expand_grid(
    subestuary  = model_config$cfg_subestuary[cntrl_subestuary],
    wq_var      = model_config$cfg_wq_var[cntrl_wq_var],
    trans_name  = model_config$cfg_trans_names[cntrl_trans_names],
    gam_id      = cntrl_gam_formula,
    gam_by_term = model_config$cfg_by_term[cntrl_by_term],
    data_source = model_config$cfg_data_source[cntrl_data_source],
    date_id     = cntrl_date) %>%
    mutate(
      formula_name = model_config$cfg_gam_formula_name[gam_id],
      date_begin   = model_config$cfg_date_range$start[date_id],
      date_end     = model_config$cfg_date_range$end[date_id]) %>%
    relocate(formula_name, .after = gam_id) %>%
    ungroup()

  if (show_msgs) {
    message(sprintf("Job Count: %d", nrow(job_que)))
  }

  return(job_que)
}## FUN ~ create_job_que


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Jon Harcum, 2024-04-11
#' @title Down Select Data Based on Job Configuration
#'
#' @description Refines and filters the input datasets according to the
#' specifications of a given job configuration. This includes selecting segments
#' , stations, and data points that match the criteria defined in the job (e.g.,
#' subestuary, time range, and data source). The function ensures that only
#' relevant data is used in each specific modeling job, optimizing the modeling
#' process and enhancing the accuracy of the results.
#'
#' @param job A list specifying the job configuration, including the selected
#' subestuary, GAM by term, data source, and the date range for the analysis.
#' @param model_data A list containing loaded and pre-processed model data.
#' @param show_msgs Logical; if `TRUE`, displays messages during the data
#' selection process.
#'
#' @return A list containing refined datasets for use in the specified job,
#' including datasets for stations, the main data set, and segments.
#'
#' @examples
#' \dontrun{
#' job_config <- list(subestuary="CB1"
#'                    , gam_by_term="year"
#'                    , data_source="main"
#'                    , date_begin=as.Date("2010-01-01")
#'                    , date_end=as.Date("2010-12-31"))
#' refined_data <- down_select_data(job=job_config, model_data=loaded_data)
#' }
#'
#' @importFrom dplyr filter mutate select arrange left_join case_when
#' @importFrom rlang sym
#' @importFrom lubridate hour minute
#' @importFrom magrittr %>%
#'
#' @export
down_select_data <- function(job = NULL, model_data = NULL, show_msgs = FALSE) {

  # QC - check that each variable is not NULL
  stopifnot(all(sapply(list(job, model_data), function(x) !is.null(x))))

  # QC - check contents of job
  stopifnot(all(c("subestuary"
                  , "gam_by_term"
                  , "data_source"
                  , "date_begin"
                  , "date_end") %in% names(job)))
  stopifnot(all(sapply(c("subestuary"
                         , "gam_by_term"
                         , "data_source"
                         , "date_begin"
                         , "date_end")
                       , function(key) {!is.null(job[[key]]) && !is.na(job[[key]])})))

  # Process segments based on subestuary
  segments <- model_data$subestuary %>%
    filter(cb4d_subestuary == job$subestuary) %>%
    mutate(reg = !!sym(job$gam_by_term)) %>%
    mutate(reg = as.factor(reg)) %>%
    select(-all_of(starts_with("by_"))) %>%
    arrange(-in_subestuary)

  if (nrow(segments) != length(unique(segments$cbseg_92))) {
    stop("cbseg_92 values in segments must be unique.")
  }

  segments_tmp <- model_data$cbseg_92_sf %>%
    filter(cbseg_92 %in% segments$cbseg_92)

  segments <- left_join(segments_tmp, segments, by="cbseg_92") %>%
    relocate(names(segments), .before = 1)

  # Process stations based on segments
  stations <- model_data$stations %>%
    filter(cbseg_92 %in% segments$cbseg_92)

  stations <- left_join(stations,
                        segments %>% select(cb4d_subestuary
                                            , cbseg_92
                                            , in_subestuary
                                            , add_data
                                            , reg),
                        by = "cbseg_92") %>%
    arrange(-in_subestuary, -fixed)

  # Process data based on stations, date range, and data source
  data <- model_data$data %>%
    filter(station %in% stations$station) %>%
    filter(between(date_time, job$date_begin, job$date_end))

  # Filter data to just datahub if called for
  if (job$data_source == "datahub") {
    data <- data %>%
      filter(source %in% "datahub")
  }

  data <- data %>%
    mutate(doy = leap_yday(date_time),
           date_d = year(date_time) + leap_yday(date_time)/366,
           hod = hour(date_time) + minute(date_time)/60)

  stations_tmp <-  stations %>%
    st_drop_geometry() %>%
    select(-latitude, -longitude, -geometry)

  data <- left_join(data,
                    stations_tmp, by = "station")

  # Refine stations to just those with data
  stations <- stations %>%
    filter(station %in% unique(data$station)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  if (show_msgs) {
    print(sprintf("Observation Counts > Segments: %d | Stations: %d | Data: %d",
                  nrow(segments), nrow(stations), nrow(data)))
  }

  return(list(stations = stations,
              data = data,
              segments = segments))
}## FUN ~ down_select_data

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Jon Harcum, 2024-04-11
#' @title Compute Day of Year Adjusted for Leap Year
#'
#' @description Calculates the day of the year (DOY) for a given date, adjusting
#'  as if every year is a leap year. This adjustment ensures consistency in day
#' numbering across years, particularly useful for analyses where maintaining a
#' consistent temporal scale is important, such as in time series modeling or
#' seasonal studies.
#'
#' @param date_chk A date or datetime object for which the adjusted day of the
#' year is calculated.
#'
#' @return An integer representing the day of the year, adjusted for leap years.
#'
#' @examples
#' \dontrun{
#' date_example <- as.Date("2023-03-01")
#' leap_yday(date_example)  # Outputs: 61
#' }
#'
#' @importFrom lubridate year yday month leap_year
#'
#' @export
# ----< Function which computes doy as though every year is leap year >---
leap_yday <- function(date_chk) {
  is_leap_year <- leap_year(year(date_chk))
  yday(date_chk) + ifelse(month(date_chk) > 2 & !is_leap_year, 1, 0)
}## FUN ~ leap_yday



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Jon Harcum, 2024-04-11
create_rda_file_name <- function(job) {

  x<-with(job, paste(wq_var,
                     subestuary,
                     data_source,
              paste(format(date_begin, "%Y"), format(date_end, "%Y"),sep = "-"),
                     formula_name,
                     gam_by_term,
                     trans_name,
                     sep = "~")
  )
  x<-paste0(x,".rda")
  return(x)

}## FUN ~ create_rda_file_name


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Jon Harcum, 2024-04-11
#' @title Initialize Model Summary Table
#'
#' @description Creates an empty tibble (data frame) structured to hold summary
#' statistics and information for modeling results. This table is designed to
#' accumulate results from multiple modeling jobs, providing a standardized
#' format for collecting key metrics such as RMSE, R-squared values, AIC, and
#' other relevant model performance indicators.
#'
#' @return A tibble with predefined columns for capturing model summary
#' statistics. Each column is initialized to hold a specific type of data
#' relevant to modeling outcomes.
#'
#' @examples
#' \dontrun{
#' model_summary_table <- initialize_model_summary_table()
#' }
#'
#' @importFrom tibble tibble
#'
#' @export
initialize_model_summary_table <- function() {

  # initialize model_summary variable
  model_summary0 <- tibble(
    subestuary       = character(0),
    wq_var           = character(0),
    year_range       = character(0),
    data_source      = character(0),
    stations         = integer(0),
    observations     = integer(0),
    datahub          = integer(0),
    eotb             = integer(0),
    vecos            = integer(0),
    noaa             = integer(0),
    formula_name     = character(0),
    trans_name       = character(0),
    gam_by_term      = character(0),
    date_begin       = as.POSIXct(character(0)),
    date_end         = as.POSIXct(character(0)),
    run_time_start   = as.POSIXct(character(0)),
    run_time_stop    = as.POSIXct(character(0)),
    run_time_elapsed = numeric(0),
    rmse             = numeric(0),
    r_sq             = numeric(0),
    aic              = numeric(0),
    trans_parms      = list(),
    gam_formula      = character(0)
  )

  return(model_summary0)

}## FUN ~ initialize_model_summary_table

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Jon Harcum, 2024-04-11
#' @title Generate Model Specifications Table
#'
#' @description Constructs a tibble containing detailed specifications of a
#' modeling job, including the selected subestuary, water quality variable, data
#' source, time range, and runtime information. This function organizes the job
#' configuration and runtime metrics into a structured format, facilitating
#' easy review and analysis of the modeling process and its parameters.
#'
#' @param job A list detailing the specific job configuration, including
#' subestuary, GAM terms, and other settings.
#' @param run_time_start The start time of the modeling job, typically recorded
#' as the job begins.
#' @param run_time_stop The stop time of the modeling job, recorded once the job
#'  completes.
#'
#' @return A tibble with columns for each key aspect of the job's specifications
#'  and runtime details, formatted for readability and further analysis.
#'
#' @examples
#' \dontrun{
#' job_config <- list(subestuary="CB1"
#'                    , wq_var="DO"
#'                    , gam_by_term="year"
#'                    , date_begin=as.Date("2020-01-01")
#'                    , date_end=as.Date("2020-12-31"))
#' specs_table <- rg_model_specs(job_config, Sys.time(), Sys.time())
#' }
#'
#' @importFrom tidyr as_tibble
#' @importFrom dplyr mutate
#' @importFrom lubridate year
#'
#' @export
rg_model_specs <- function(job, run_time_start, run_time_stop) {

  rg_mod_specs <- as_tibble(job) %>%
    select(-any_of(c("gam_id", "date_id"))) %>%
    mutate(year_range = paste0(year(job$date_begin), "-", year(job$date_end)),
           run_time_start = run_time_start,
           run_time_stop = run_time_stop,
           run_time_elapsed = as.numeric(run_time_stop- run_time_start
                                         , units = "mins"))

  return(rg_mod_specs)
}## FUN ~ rg_model_specs

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Jon Harcum, 2024-04-11
#' @title Count Observations by Source in Data Frame
#'
#' @description Aggregates and counts observations within a given data frame,
#' categorizing counts by data source. This function is particularly useful for
#' summarizing the number of observations from different sources in
#' environmental and water quality datasets, providing a quick overview of data
#' availability and source
#' distribution.
#'
#' @param df A data frame containing environmental or water quality
#' observations, including a 'source' column to indicate the data source for
#' each observation.
#'
#' @return A tibble summarizing the total number of stations, observations, and
#' counts of observations categorized by source (e.g., 'datahub', 'eotb',
#' 'vecos', 'noaa').
#'
#' @examples
#' \dontrun{
#' observation_summary <- rg_observation_count(data_frame)
#' }
#'
#' @importFrom tibble tibble
#'
#' @export
rg_observation_count <- function(df) {

  # QC - check contents of job
  stopifnot(all(c("station", "source") %in% names(df)))

  # compute observation counts
  counts <- tibble(stations     = length(unique(df$station)),
                   observations = nrow(df),
                   datahub      = sum(df$source=="datahub"),
                   eotb         = sum(df$source=="eotb"),
                   vecos        = sum(df$source=="vecos"),
                   noaa         = sum(df$source=="noaa"))

  return(counts)
}## FUN ~ rg_observation_count


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Jon Harcum, 2024-04-11
#' @title Calculate Model Statistics
#'
#' @description Computes key statistical metrics for a model based on residuals
#' and observed values. This function calculates the root mean square error
#' (RMSE), R-squared value, and Akaike Information Criterion (AIC) for a given
#' set of model outputs, aiding in the evaluation of model performance.
#'
#' @param gs The fitted model object from which statistics will be calculated.
#' @param df A data frame containing the observed values and residuals from the
#' model.
#'
#' @return A tibble with calculated statistical metrics for the model, including
#'  RMSE, R-squared, and AIC.
#'
#' @examples
#' \dontrun{
#' stats <- rg_model_statistics(fitted_model, model_data_frame)
#' }
#'
#' @importFrom tibble tibble
#' @importFrom stats AIC
#'
#' @export
rg_model_statistics <- function(gs, df) {

  # model statistics
  num_rec <- nrow(df)
  sst     <- sum((df$y_obs - mean(df$y_obs))^2)    # Total Sum of Squares
  sse     <- sum((df$y_resid)^2)                   # Sum of Squared Errors

  df_stats <- tibble(rmse    = sqrt(sse/num_rec),                     # RMSE
                     r_sq    = 1 - (sse / sst),                       # R^2
                     aic     = AIC(gs))

}## FUN ~ rg_model_statistics

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Jon Harcum, 2024-04-11
#' @title Execute GAM Modeling Job
#'
#' @description Facilitates the execution of a Generalized Additive Model (GAM)
#' job based on specified #' parameters and data. This function sets up the data
#' , applies any required transformations, runs the GAM, and optionally
#' post-processes the output to include predictions and residuals. It is
#' designed to streamline the modeling process, from data preparation through to
#' model evaluation.
#'
#' @param job A list detailing the job configuration, including the model
#' formula, data transformations, and variables.
#' @param data A data frame containing the dataset to be used in the model.
#' @param model_config A list containing the model configuration settings.
#' @param post_process Logical; if `TRUE`, the function will append predictions
#' and calculate residuals.
#' @param show_msgs Logical; if `TRUE`, progress messages will be displayed.
#'
#' @return The fitted model object, potentially augmented with additional data
#' such as predictions and residuals if post-processing is enabled.
#'
#' @examples
#' \dontrun{
#' fitted_model <- run_gam(job_config, data_frame, model_config_settings)
#' }
#'
#' @importFrom dplyr filter mutate
#' @importFrom magrittr %>%
#' @importFrom mgcv gam
#' @importFrom lubridate year month
#'
#' @export
run_gam <- function(job = NULL
                    , data = NULL
                    , model_config = NULL
                    , post_process = TRUE
                    , show_msgs = TRUE) {

  # set up data (wq_var & transformations); run gam
  {
    # start clock
    run_time_start <- Sys.time()

    # QC - check that each variable is not NULL
    stopifnot(all(sapply(list(job, data, model_config), function(x) !is.null(x))))

    # QC - check job to make sure it has all needed variables
    stopifnot(all(c("formula_name", "trans_name", "wq_var") %in% names(job)))
    stopifnot(all(sapply(c("formula_name", "trans_name", "wq_var"), function(key) {!is.null(job[[key]]) && !is.na(job[[key]])})))

    # extract gam formula from model_config by knowing formula_name
    gam_formula_idx <- which(model_config$cfg_gam_formula_name == job$formula_name)
    if (is_empty(gam_formula_idx)) {stop("run_gam: GAM formula not found")}
    gam_form <- model_config$cfg_gam_formula[gam_formula_idx]

    # extract transformation parameters by knowing transformation name
    trans_name_idx <- which(model_config$cfg_trans_names == job$trans_name)
    if (is_empty(trans_name_idx)) {stop("run_gam: Transformation not found")}
    trans_parms <- model_config$cfg_trans_parms[[trans_name_idx]]

    # Based on the above, position dependent variable (e.g., wq_var) as "y_obs"
    # and apply appropriate transformations to create "y" which is used in gam formula
    df <- data %>%
      mutate(y_obs = !!sym(job$wq_var)) %>%
      filter(!is.na(y_obs)) %>%
      mutate(y = transform_data(y_obs, job$trans_name, trans_parms, show_msgs=TRUE))

    # QC - confirm df has all variables that appear in the gam formula, gam_form
    variable_list <- all.vars(as.formula(gam_form))
    if (!all(variable_list %in% names(df))) {message(paste("Warning: run_gam: ",paste0((variable_list[!(variable_list %in% names(df))]), collapse = ", "), "not found in data"))}

    if (show_msgs) {
      cat(sprintf("Subest: %-5s Var: %-6s Data: %-7s | %s-%s   Model: %-15s | %-10s Trans: %-10s Start: %-15s",
                  job$subestuary, job$wq_var, job$data_source,
                  substr(job$date_begin, 1, 4), substr(job$date_end, 1, 4),
                  job$formula_name, job$gam_by_term,
                  job$trans_name, format(as.POSIXct(run_time_start, format="%Y-%m-%d %H:%M:%S"), "%Y-%m-%d %H:%M")
      ),
      sep = "", fill = FALSE)
    }

    # run mgcv::gam with error trapping
    {
      gs <- tryCatch({
        gam(as.formula(gam_form), data = df)
      }, error = function(e) {
        cat("An error occurred: ", conditionMessage(e), "\n")
        NULL  # Return NULL which will be assigned to 'gs'
      })
    }

    # stop the clock
    run_time_stop <- Sys.time()
  }

  # post process gam output and append to the list gs
  if (!is.null(gs) ) {

    # add prediction to the df in "observed" units by back transforming predictions
    # and then compute residuals in observed units
    df <- df %>%
      mutate(y_pred = as.vector(transform_data_inverse(predict(gs), job$trans_name, trans_parms)),
             y_resid = y_obs - y_pred)

    # consolidate model specifications, observation counts, and statistics
    rg_mod_specs  <- rg_model_specs(job, run_time_start, run_time_stop)
    rg_obs_cnt    <- rg_observation_count(df)
    rg_mod_stats  <- rg_model_statistics(gs, df)
    rg_model      <- bind_rows(initialize_model_summary_table(),
                               bind_cols(rg_mod_specs, rg_obs_cnt, rg_mod_stats))
    rg_model$trans_parms <- list(trans_parms)   # paste(trans_parms, collapse = ", ")
    rg_model$gam_formula <- gam_form

  }

  if (!is.null(gs) && post_process) {
    gs[c("cb4d_data", "cb4d_model_summary")]  <- mget(c("df", "rg_model"))
  }

  if (!is.null(gs) && show_msgs)  {
    cat(sprintf(" RMSE: %6s R2: %6s AIC: %6s\n",
                signif(rg_model$rmse,5), signif(rg_model$r_sq,4), round(rg_model$aic,1)),
    sep = "", fill = FALSE)

  }

  return(gs)

}## FUN ~ run_gam

