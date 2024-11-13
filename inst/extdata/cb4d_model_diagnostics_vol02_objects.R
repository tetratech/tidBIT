
# VOLUME 2: Base notebook
# make a notebook that can be pushed to i) html or ii) word .docx  (2024-02-24)
# Rather than put code in RMD file code is in R script
# The RMD file reads the objects created here
# Load files differently if scripted vs. interactive

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# If running interactive modify model (gs_file) in IF statement
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# *** JBH03.27: cleaned up/documented selected functions

# Packages and other----
# source("diagnostics_scripts/libraries_and_settings.R")
# source("diagnostics_scripts/fun_beta_logit.R")
# source("diagnostics_scripts/fun_cb4d_buildout_00.R")
# source("diagnostics_scripts/fun_cb4d_buildout_01.R")
# source("diagnostics_scripts/fun_cb4d_diagnostics_00.R")
# source("diagnostics_scripts/fun_mapping.R")
# source("diagnostics_scripts/fun_mapping_tools.R")
# source("diagnostics_scripts/settings_Excel.R")

# # Load Model----
# ## Load Method ----
# ## Loads with variable set in report_diagnostic()
# ### else loads user files
# # *** JBH03.27: commented to facilitate testing
# if (run_type == "script") {
#   # Load RDA via report_diagnostic() function
#   # **DO NOT** EDIT this part of the IF statement!
#   # i defined in calling function
#   # Remove some left over objects if this is not the first segment
#   #if (exists("cb4d_segments_sf")) {rm(cb4d_segments_sf)}
# } else {
#   # run_type == "interactive"
#   # Load RDA (model run): file name
#   # gs_file <- "gs~CB3~all~2005_2008~08_16~2023_10_23_005~by_term2.rda"
#   # Load RDA (model run): tcltk
#   path_model_rda  <- tcltk::tk_choose.files()
#   dn_model_rda    <- dirname(path_model_rda)
#   fn_model_rda    <- basename(path_model_rda)
#   gs_file <- fn_model_rda # using tcltk from run_reports.R
#   #fn_rmd <- paste0("report_vol1~", tools::file_path_sans_ext(gs_file))
#   dn_models <- file.path("cb4d_model", "cb4d_gam_models")
#   gs_path <- file.path(dn_models, gs_file)
#   # gs_path <- file.choose()
#   i <- gs_path
# }## IF ~ run_type

## Load ----
# i<- "cb4d_gam_models/do~CB3~all~2016-2018~2023_10_07_005~by_term2~none.rda"
# i<- "cb4d_gam_models/do~CB3~all~2016-2018~2023_10_07_005~by_term2~BL_010_060.rda"
load(i)


# *** JBH03.27: revised/combined "add on" variables

## data ----
pry(gs, c("cb4d_data"
          , "cb4d_model_summary"
          , "cb4d_segments"
          , "cb4d_stations"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Numbered sections reference RMD notebook
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Ensure any objects created here are referenced in the RMD if want them included

# 1.0 meta information about model----
{
  title_ith <- gsub("\\.rda$"
                    , ""
                    , paste(strsplit(basename(i), "~")[[1]], collapse = " | "))

  # *** JBH03.27: need to process dates and list in this tibble for output
  cb4d_model_summary_fmt <- cb4d_model_summary %>%
    mutate(across(where(is.POSIXct), ~format(.x, "%Y-%m-%d %H:%M:S %Z"))) %>%
    mutate(across(where(is.list), ~paste(trans_parms, collapse = ", ")  ))
  data_01 <- tibble(FieldName = names(cb4d_model_summary_fmt)
                    , Value = as.vector(unlist(cb4d_model_summary_fmt)))
  tbl_01 <- tblFT1(data_01
                   , tbl_title = title_ith
                   , tbl_theme = "box")
  tbl_01


}## 1.0

# 2.0 model map----
{

  # *** JBH03.27: vastly revised functions
  M2 <- map_base() %>%
    map_polygon(sf_obj = cb4d_segments, obj_name = "cbseg_92",
                col_name = "in_subestuary", col_palette = c("goldenrod", "blue"),
                legend_title = "In Subestuary") %>%
    map_points(sf_obj = cb4d_stations, obj_name = "station",
               col_name = "fixed", col_palette = c("black", "red"),
               legend_title = "Fixed Station")
  print(M2)

  M2_static  <- ggplot2::ggplot(cb4d_segments) +
    ggplot2::geom_sf(color = "blue") +
    ggplot2::geom_sf(data = cb4d_stations, color = "red") +
    ggplot2::labs(title = cb4d_model_summary$subestuary)

  # print(M2_static)

}## 2.0

# 3.0 model summary----
{
  # substantively want what we get from
  # summary(gs)

  # *** JBH03.27: added tbl_03_00
  tbl_03_00 <- tblFT1(tibble(gam_formula = cb4d_model_summary_fmt$gam_formula))
  tbl_03_00

  gs_gamCoeff <- gamCoeff(gs)
  tbl_03_01 <- tblFT1(gs_gamCoeff, tbl_title = "Parametric Coefficients")
  tbl_03_01

  gs_ANOVA <- gamANOVA(gs)
  tbl_03_02 <- tblFT1(gs_ANOVA, tbl_title = "Approximate significance of smooth terms:")
  tbl_03_02

}## 3.0

# 4.0 statistics by station and 5.0 NEW----
{



  station_vec <- filter(cb4d_data, in_subestuary == TRUE) %>%
    select(station) %>%
    unique() %>%
    unlist(use.names = FALSE)

  # Borrow depths table (7.0) from Volume 1 report
  cross_tab <- left_join(cb4d_data %>%
                           # select(station) %>%
                           group_by(station, in_subestuary, fixed, reg, cbseg_92) %>%
                           summarise(num_rec_tot = n(), .groups = 'drop') ,
                         cb4d_data %>%
                           mutate(depth_grp = round(depth, 0)) %>%
                           group_by(station, depth_grp) %>%
                           summarise(num_rec = n(), .groups = 'drop') %>%  # varied depths
                           pivot_wider(names_from = depth_grp
                                       , values_from = num_rec
                                       , values_fill = list(n = 0)
                                       , names_sort = TRUE) ,
                         by = "station") %>%
    arrange(desc(in_subestuary), desc(fixed), reg, desc(num_rec_tot), station)

  tbl_cross_tab <- tblFT1(cross_tab
                          , tbl_title = "Observation count by station and depth"
                          , tbl_theme = "box")
  tbl_cross_tab

  # desired depths
  depth_target <- c(1, 6, 12, 18, 24, 30)
  # surface, above picnocline, below picnocline, bottom, bottom, bottom)

  len_station_vec <- length(station_vec)
  seq_len_station_vec <- seq_len(len_station_vec)
  ls_faceted <- vector("list", len_station_vec)

  col_drop <- c("station"
                , "in_subestuary"
                , "fixed"
                , "reg"
                , "cbseg_92"
                , "num_rec_tot")

  # 5, variables
  start_date_time = cb4d_model_summary$date_begin
  stop_date_time  = cb4d_model_summary$date_end
  time_step = 1
  ls_p5 <- vector("list", len_station_vec)
  order <- "station"


  ls_p5_dat <- vector("list", len_station_vec)
  names(ls_p5_dat) <- station_vec


  ## Loop ----
  for (s in seq_len_station_vec) {
    # s<-1
    cat(paste0(s,"\n"))
    s_name <- station_vec[s]

    ## depth vector ----
    cross_tab_s <- filter(cross_tab, station == s_name) %>%
      select_if(~ !any(is.na(.)))

    s_depth <- as.numeric(names(cross_tab_s)[!names(cross_tab_s) %in% col_drop])

    s_depth_n <- select(cross_tab_s, as.character(s_depth)) %>%
      unlist(use.names = FALSE)

    s_depth_n_total <- select(cross_tab_s, num_rec_tot) %>%
      unlist(use.names = FALSE)

    s_depth_max <- max(s_depth)
    # limit depth_target to station
    depth_target_s <- depth_target[depth_target <= s_depth_max]

    # check to ensure present
    if (sum(depth_target_s %in% s_depth) == length(depth_target_s)) {
      # do nothing
    } else {
      # replace missing depths with next highest

      ## 1
      if (!1 %in% s_depth) {
        depth_target_s[match(1, depth_target_s)] <- 0
      }## IF ~ 1

      ## 6, 12, 18, 24, 30
      # replace with next highest depth in the interval
      for (dt in depth_target[-1]) {
        d_num <- dt
        d_num_prev <- depth_target[match(d_num, depth_target) - 1]
        # add max measured if between targets
        if (median(c(d_num_prev, s_depth_max, d_num)) == s_depth_max) {
          depth_target_s <- c(depth_target_s, s_depth_max)
        } ## IF ~ median
      }## FOR ~ dt

    }## IF ~ target depths


    # add max greater than target max
    # keep to 6 measurements at most
    if (s_depth_max > max(depth_target_s)) {
      if (length(depth_target_s) == 6){
        depth_target_s[6] <- s_depth_max
      } else {
        depth_target_s <- c(depth_target_s, s_depth_max)
      }## IF ~ len 6
    }## IF ~ max

    # something off so just use unique
    depth_target_s <- unique(depth_target_s)


    # get depths for plot
    depth_vec <- depth_target_s[depth_target_s %in% s_depth]


    ## 4, plot ----
    # create plots and save to list

    # p4 <- plot_faceted(data = cb4d_data
    #                    , s_name
    #                    , depth_vec
    #                    , order = "station")
    # if (format_rmd == "html") {
    #   ls_faceted[[s]] <- try(plot_faceted(data = cb4d_data
    #                                       , s_name
    #                                       , depth_vec
    #                                       , order = "station"))
    # } else {
    #   ls_faceted[[s]] <- try(plot_faceted(data = cb4d_data
    #                                       , s_name
    #                                       , depth_vec
    #                                       , order = "station"))
    # }## IF ~ format_rmd
    if (format_rmd == "html") {
      ls_faceted[[s]] <- try(plot_obs_pred_res(
        df = subset(cb4d_data, station == s_name),
        main = s_name,
        smooth_line = TRUE,
        smooth_line_width = 1.5
      ))


    } else {
      ls_faceted[[s]] <- try(plot_obs_pred_res(
        df = subset(cb4d_data, station == s_name),
        main = s_name,
        smooth_line = TRUE,
        smooth_line_width = 1.5
      ))


    }## IF ~ format_rmd


     ## 5, prediction ----

    #~~~~~~~~~~~   make function: make_prediction_data_set
    # first make prediction data set

      p_dat_01 <- cb4d_stations %>%
        select(station, depth_b, wb_lat_km, wb_lon_km, reg) %>%
        filter(station %in% s_name) %>%
        st_set_geometry(NULL)

      p_dat_02 <- tibble(depth = depth_target_s)

      p_dat_03 <- generate_date_time(start_date_time ,
                                     stop_date_time ,
                                     time_step , time_units = "days", tz = "EST") %>%
        mutate(doy = leap_yday(date_time),
               date_d = year(date_time) + leap_yday(date_time)/366)

      p_dat <- crossing(p_dat_01, p_dat_02, p_dat_03)

      # p_dat <- p_dat %>%
      #   filter(depth <= s_depth_max) # <- do not allow predictions deeper than station (see XHH4931)


      #~~~~~~~~~~~   make function: make_predictions

      # here's a quick qc to confirm that p_dat has all the independent variables
      variable_list <- all.vars(gs$formula)[-1]
      if (!all(variable_list %in% names(p_dat))) {
        message(paste("Warning: run_gam: "
                      , paste0((variable_list[!(variable_list %in% names(p_dat))])
                               , collapse = ", ")
                      , "not found in data"))
        }## IF ~ all variables

      # here's the syntax to make predictions
      p_dat$y_pred <- transform_data_inverse(predict(gs, newdata=p_dat, se.fit=FALSE),
                                             cb4d_model_summary$trans_name,
                                             unlist(cb4d_model_summary$trans_parms))


      #
      # Create a new column that combines station and depth
      data <- cb4d_data %>%
        mutate(depth_grp = round(depth, 0)) %>%
        filter(station %in% s_name, depth_grp %in% depth_target_s) %>%
        mutate(station_depth = paste0("Station: ", station, ", Depth: ", depth_grp, " m"))


      # Convert station_depth to a factor and order it by depth
      if (order == "station") {
        data$station_depth <- factor(data$station_depth
                                     , levels = unique(data[order(data$station, data$depth_grp),]$station_depth))
      } else {
        data$station_depth <- factor(data$station_depth
                                , levels = unique(data[order(data$depth_grp, data$station),]$station_depth))
      }


      # Create a new column that combines station and depth
      pred <- p_dat %>%
        filter(station %in% s_name, depth %in% depth_target_s) %>%
        mutate(station_depth = paste0("Station: ", station, ", Depth: ", depth, " m"))

      # Convert station_depth to a factor and order it by depth
      if (order == "station") {
        pred$station_depth <- factor(pred$station_depth
                                     , levels = unique(pred[order(pred$station
                                                , pred$depth),]$station_depth))
      } else {
        pred$station_depth <- factor(pred$station_depth
                                     , levels = unique(pred[order(pred$depth
                                              , pred$station),]$station_depth))
      }



      ls_p5[[s]] <- try(ggplot(data, aes(x = date_time, y = y_obs)) +
                          geom_point(color = "salmon", shape = 1, alpha = 1) +
                          geom_line(data = pred, aes(x = date_time, y = y_pred), color = "black") +
                          facet_wrap(~station_depth, scales = "free_x") +
                          theme_minimal() +
                          theme(panel.border = element_rect(color = "black"
                                                            , fill = NA
                                                            , linewidth = 0.25)
                                , strip.text = element_text(size = 8)) +
                          xlab("Date") +
                          ylab("") )

      ls_p5_dat[[s]] <- pred

  }## FOR ~ s


  # write xlsx for p5 data
  fn_wb <- file.path(dn_results
                     , paste0(fn_report_prefix
                              , tools::file_path_sans_ext(basename(i))
                              , "_Predictions.xlsx")
  )
  openxlsx::write.xlsx(ls_p5_dat, fn_wb)


}## 4.0 and 5.0

# end Volume II
