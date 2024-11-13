# new stuff for notebooks
# Erik, 20240910
# move to other files later
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Prepare table of coefficients for GAM analysis
#'
#' @param lmo output from gam model
#' @keywords internal
#' @export
#'
gamCoeff <- function(lmo) {

  # lmo <- gs

  lm.sum <- summary(lmo)
  p.se <- lm.sum$se[1:length(lm.sum$p.coeff)]
  lm.coeff <- data.frame(
    source    = names(lm.sum$p.coeff),
    estimate  = round(lm.sum$p.coeff,6),
    std.error = round(p.se,6),
    t.value   = round(lm.sum$p.t,4),
    p.value   = .fmtPval(lm.sum$p.pv), stringsAsFactors = FALSE)
  rownames(lm.coeff) <- NULL

  return(lm.coeff)
}


#' Format pvalues
#'
#' @param pval pvalue to format
#' @keywords internal
#' @export
#'
.fmtPval <- function(pval) {

  pval <- ifelse((pval<0.0001),"<0.0001",sprintf('%6.4f',pval))
  pval[is.na(pval)] <- "-"
  fmtPval <- pval
}


#' Prepare ANOVA table for GAM analysis
#'
#' @param lmo output from gam model
#' @keywords internal
#' @export
gamANOVA <- function(gamo) {

  # gamo <- gs

  anov.gamo     <- anova(gamo)
  if (length(anov.gamo$pTerms.table)>0) {
    agamp <- data.frame(anov.gamo$pTerms.table)
  } else {
    agamp <- data.frame(df=NA_real_,F=NA_real_,p.value=NA_real_)
    rownames(agamp) <- "NA"
  }
  agamp$type    <- '   "      " '
  agamp$type[1] <- 'parametric terms'
  agams         <- data.frame(anov.gamo$s.table)
  agams$type    <- '   "      " '
  agams$type[1] <- 'smoothed terms'
  names(agams)[names(agams)=='edf'] <- 'df' # rename edf
  agams         <- agams[,names(agams)!="Ref.df"]   # drop ref.df
  agamo         <- rbind(agamp,agams)
  agamo$source  <- rownames(agamo)
  agamo[,1]     <- round(agamo[,1],2)  # round edf
  agamo[,2]     <- round(agamo[,2],4)  # round F-stat
  agamo[,3]     <- .fmtPval(agamo[,3]) # format p-value
  rownames(agamo) <- NULL
  agamo <- agamo[,c(c(4,5,1,2,3))]
  return(agamo)
}


# ----< Function: generate_date_time >---
# model diagnostics report, Vol 2
#' generate date time
#' @param start_date_time starting date and time
#' @param stop_date_time ending date and time
#' @param time_step time increment, Default = 1
#' @param time_units time units, Default = "days"
#' @param tz time zone, Default = "EST"
#' @keywords internal
generate_date_time <- function(start_date_time
                               , stop_date_time
                               , time_step = 1
                               , time_units = "days"
                               , tz = "EST") {

  # Ensure the start and stop date_times are in POSIXct format with the specified timezone
  start_date <- as.POSIXct(start_date_time, tz = tz)
  stop_date <- as.POSIXct(stop_date_time, tz = tz)

  # Generate the sequence of dates based on the specified units
  if (time_units == "days") {
    time_step <- time_step * 86400 # Convert days to seconds
  } else if (time_units == "hours") {
    time_step <- time_step * 3600 # Convert hours to seconds
  }
  date_time <- seq(from = start_date, to = stop_date, by = time_step)

  # Create a tibble
  dates_tibble <- tibble::tibble(date_time = as.POSIXct(date_time, origin = "1970-01-01", tz = tz))

  return(dates_tibble)
}

# *** JBH 05.20: updated to address map palette se
#' @keywords internal
get_palette <- function(column, true_color, false_color) {
  unique_values <- unique(column)
  if (length(unique_values) == 1) {
    return(ifelse(unique_values == TRUE, true_color, false_color))
  } else {
    return(c(true_color, false_color))
  }
}## get_palette

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# create report objects
# 20241108
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' @title CB4D Diagnostics Report, Create Objects
#'
#' @description Create objects for diagnostics report for CB4D model
#'
#' @details
#'
#' Creates objects for use with the diagnostic report.
#'
#' Input is a path and file name.  The file is a script that creates the objects
#' used in the report.  The data it uses must be in the global environment.
#'
#' This function replaces the "source" command in the original version of the
#' scripts to create the report objects.
#'
#' Objects for volume "1" and "2" only.
#'
#' @param report_version Version of report (1 or 2).  Default = NULL
#'
#' @keywords internal
#'
#' @export
report_objects <- function(report_version = NULL) {

  # QC ----
  if (is.null(report_version)) {
    msg <- paste0("No report version specified.\n",
                  "Accepted values are 1 or 2.")
    stop(msg)
  }## IF ~ NULL

  # Excel Settings (openxlsx)
  # 2024-03-22
  # Erik.Leppo@tetratech.com
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Copy here to not clutter main script
  # For use with baytrends 4d diagnostic reports
  # Copy tables from report to table
  # openxlsx requires some setup (e.g., styles)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # not including a NOTES worksheet as this time

  # Excel Global ----
  # Excel Settings ---
  xl_SR <<- 8 # number of rows to skip for new worksheets
  xl_SR_trans <<- 2 # for transposed df, skip worksheet title

  # Excel, Formatting ----
  ## Excel, Formatting, Styles ----
  style_title <<- openxlsx::createStyle(fontName = "Cambria"
                                       , fontSize = 18
                                       , fontColour = "#1F497D"
                                       , textDecoration = "bold")
  style_h1 <<- openxlsx::createStyle(fontName = "Calibri"
                                    , fontSize = 15
                                    , fontColour = "#1F497D"
                                    , textDecoration = "bold"
                                    , border = "Bottom"
                                    , borderColour = "#4F81BD"
                                    , borderStyle = "thick")
  style_h2 <<- openxlsx::createStyle(fontName = "Calibri"
                                    , fontSize = 13
                                    , fontColour = "#1F497D"
                                    , textDecoration = "bold"
                                    , border = "Bottom"
                                    , borderColour = "#A7BFDE"
                                    , borderStyle = "thick")
  style_hyperlink <<- openxlsx::createStyle(fontName = "Calibri"
                                           , fontSize = 11
                                           , fontColour = "#0000FF"
                                           , textDecoration = "underline")
  style_bold <<- openxlsx::createStyle(textDecoration = "bold")
  style_date <<- openxlsx::createStyle(numFmt = "DATE")
  style_halign_center <<- openxlsx::createStyle(halign = "center")

  # Create Objects ----

  if (report_version == 1) {
    ## VOLUME 1: Base notebook----
    load(i)


    # *** JBH03.27: revised/combined "add on" variables

    ### data ----
    pry(gs, c("cb4d_data"
              , "cb4d_model_summary"
              , "cb4d_segments"
              , "cb4d_stations"))

    ### 1.0 meta information about model----
    {
      title_ith <- gsub("\\.rda$"
                        , ""
                        , paste(strsplit(basename(i), "~")[[1]], collapse = " | "))


      # *** JBH03.27: need to process dates and list in this tibble for output
      cb4d_model_summary_fmt <- cb4d_model_summary %>%
        dplyr::mutate(dplyr::across(dplyr::where(lubridate::is.POSIXct), ~format(.x, "%Y-%m-%d %H:%M:S %Z"))) %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.list), ~paste(trans_parms, collapse = ", ") ))
      data_01 <- tibble::tibble(FieldName = names(cb4d_model_summary_fmt)
                        , Value = as.vector(unlist(cb4d_model_summary_fmt)))
      tbl_01 <<- tblFT1(data_01
                       , tbl_title = title_ith
                       , tbl_theme = "box")
      tbl_01

    }## 1.0


    ### 2.0 model map----
    {

      # # *** JBH03.27: vastly revised functions
       # M1 <<- map_base() %>%
       #   map_polygon(sf_obj = cb4d_segments,
       #               obj_name = "cbseg_92",
       #               col_name = "in_subestuary",
       #               col_palette = c("goldenrod", "blue"),
       #               legend_title = "In Subestuary") %>%
       #   map_points(sf_obj = cb4d_stations,
       #              obj_name = "station",
       #              col_name = "fixed",
       #              col_palette = c("black", "red"),
       #              legend_title = "Fixed Station")
       # print(M1)

      # *** JBH 05.20: updated to address map palette se
      # moved get_palette as a stand alone function

      # Get palettes for in_subestuary and fixed columns
      in_subestuary_palette <- get_palette(cb4d_segments$in_subestuary,
                                           "goldenrod",
                                           "blue")
      fixed_palette <- get_palette(cb4d_stations$fixed,
                                   "black",
                                   "red")

      # Create the map
      M1 <<- map_base() %>%
        map_polygon(sf_obj = cb4d_segments,
                    obj_name = "cbseg_92",
                    col_name = "in_subestuary",
                    col_palette = in_subestuary_palette,
                    legend_title = "In Subestuary") %>%
        map_points(sf_obj = cb4d_stations,
                   obj_name = "station",
                   col_name = "fixed",
                   col_palette = fixed_palette,
                   legend_title = "Fixed Station")

      print(M1)

      #
      # ## Map, data munge ----
      # if (!exists("cb4d_segments_sf")) {
      #   # variable doesn't exist
      #   # load segments and filter for current model
      #   load("data/cb_cbseg92_sf.rda")
      #   cb4d_segments_sf <- cb_cbseg92_sf[cb_cbseg92_sf$cbseg_92 %in%
      #                                       cb4d_segments$cbseg_92, ]
      # }## IF ~ is.null(cb4d_segments_sf)
      #
      # if (is.null(cb4d_segments_sf)) {
      #   # variable is blank
      #   # load segments and filter for current model
      #   load("data/cb_cbseg92_sf.rda")
      #   cb4d_segments_sf <- cb_cbseg92_sf[cb_cbseg92_sf$cbseg_92 %in%
      #                                       cb4d_segments$cbseg_92, ]
      # }## is.null("cb4d_segments_sf")
      #
      # # sites in main focus vs not - color code different
      # cb4d_stations_1fixed <- filter(cb4d_stations, fixed == TRUE)
      # cb4d_stations_2other <- filter(cb4d_stations, fixed == FALSE)
      # # same with segments
      # # outliers make less prevalent
      #
      # ## Map, Leaflet----
      # M1 <- map_base() %>%
      #   map_polygon(sf_obj = cb4d_segments_sf, col_name = "cbseg_92") %>%
      #   map_points(sf_obj = cb4d_stations, col_name = "station") %>%
      #   addLegend(title = cb4d_model_overview$usr_estuary
      #             , colors = "red"
      #             , labels = 'Stations')
      #
      #   print(M1)

      # enhancements:
      # differentiate "Primary" vs "Surrounding" segments (Primary: cb4d_segments$add_data == FALSE)
      # differentiate "Fixed" vs "Supporting" stations (Fixed: cb4d_stations$fixed == TRUE)

      ## Map, ggplot----
      # Create static version for Word
      ## Leaflet won't display in Word doc
      ## *Update to match newest map (2024-03-25)----
      M1_static  <<- ggplot2::ggplot(cb4d_segments) +
        ggplot2::geom_sf(color = "blue") +
        ggplot2::geom_sf(data = cb4d_stations, color = "red") +
        ggplot2::labs(title = cb4d_model_summary$subestuary)

      print(M1_static)

    }## 2.0

    ### 3.0 model summary----
    {
      # substantively want what we get from
      # summary(gs)

      # *** JBH03.27: added tbl_03_00
      tbl_03_00 <<- tblFT1(tibble::tibble(gam_formula = cb4d_model_summary_fmt$gam_formula))
      tbl_03_00

      gs_gamCoeff <- gamCoeff(gs)
      tbl_03_01 <<- tblFT1(gs_gamCoeff
                          , tbl_title = "Parametric Coefficients")
      tbl_03_01

      gs_ANOVA <- gamANOVA(gs)
      tbl_03_02 <<- tblFT1(gs_ANOVA
                          , tbl_title = "Approximate significance of smooth terms:")
      tbl_03_02

    }## 3.0

    ### 4.0 statistics by station----
    {

      # *** JBH03.27: this step is built in the generation

      # # data operation to get data into position
      # ## add "fixed" from stations
      # cb4d_data <- bind_cols(cb4d_data,
      #                        y = gs$y, # <- technically these are also cb4d_data$do ... so a good qc
      #                        y_pred = gs$fitted.values,
      #                        y_resid = gs$residuals) %>%
      #   left_join(y = cb4d_stations[, c("station", "fixed", "cbseg_92")]
      #             , by = "station"
      #             )

      # table to be created

      df_stats <- cb4d_data %>%
        dplyr::group_by(station, in_subestuary, fixed, reg, cbseg_92) %>%
        dplyr::summarize(num_rec = dplyr::n()
                  , sse = sum((y_obs - y_pred)^2)
                  , sst = sum((y_obs - mean(y_obs))^2)
                  , rmse = sqrt(mean((y_obs - y_pred)^2))
                  , r_sq = 1 - (sse / sst)
                  , .groups = "drop_last") %>%
        dplyr::arrange(desc(in_subestuary),  desc(fixed), reg, desc(num_rec), station)
      tbl_04_01 <<- tblFT1(df_stats
                          , tbl_title = "Summary statistics"
                          , tbl_theme = "box")
      tbl_04_01

      # table columns: source station fixed cb_seg92 surrounding num_rec*  rmse* r_sq*
      # * see below equations
      # sort by i) fixed == TRUE first, ii) primary == TRUE first, iii) num_rec descending


      # basic equations to be applied on a station by station basis (not overall)
      #    num_rec <- number of records in table
      #    r_sq <- 1 - (sse / sst)
      #    sse <- sum((y - y_pred)^2) # Sum of Squared Errors
      #    sst <- sum((y - mean(y))^2) # Total Sum of Squares
      #    rmse <- sqrt(mean((y - y_pred)^2))

    }## 4.0



    ### 5.0 simple diagnostic plots (these should be ok for now)----
    {
      # unable to convert either to plotly (some features not enabled)

      plot_05_01 <<- plot_4_panel_distr(cb4d_data$y_obs, vlab = "Observed", main = title_ith, plot_type = "thin")
      # plot_4_panel_distr(cb4d_data$y_obs, vlab = "Observed", main = title_ith)
      plot_05_01


      plot_05_02 <<- plot_obs_pred_res(cb4d_data, main = title_ith, plot_type = "thin", smooth_line=TRUE)
      # plot_obs_pred_res(cb4d_data, main = title_ith, plot_type = "hex", smooth_line=TRUE)
      # plot_obs_pred_res(cb4d_data, main = title_ith, smooth_line=TRUE)
      plot_05_02

      # enhancements (plots by "in_subestuary"):
      # plot_obs_pred_res(cb4d_data[cb4d_data$in_subestuary,], main = title_ith, plot_type = "all", smooth_line=TRUE)
      # plot_obs_pred_res(cb4d_data[cb4d_data$in_subestuary & cb4d_data$fixed,], main = title_ith, plot_type = "all", smooth_line=TRUE)

    }## 5.0


    ### 6.0 gratia::draw----
    {
      # # loop through as many draw as needed to go through all n_plots_to_make plots
      # gratia::draw(gs, select = c(1, 2, 3, 4, 5, 6), rug = FALSE)
      # # ...
      # gratia::draw(gs, select = c(31, 32, 33, 34), rug = FALSE)

      df_gs_anova <- data.frame(anova(gs)$s.table)
      n_plots_to_make <- nrow(df_gs_anova)
      group_size <- 4
      plot_groups <- 1 + (((1:n_plots_to_make) - 1) %/% group_size)
      group_n <- length(unique(plot_groups))
      group_n_seqlen <<- seq_len(group_n)
      df_plots_groups <- data.frame(plot_n = seq_len(n_plots_to_make)
                                    , plot_groups)

      boo_qc <- FALSE
      if (boo_qc) {
        # QC
        table(plot_groups)
        # QC, figure out where fails
        ls_gratia <- vector("list", n_plots_to_make)
        aa_bad <- NA # c(24, 32, 35:68) # NA
        for (aa in seq_len(n_plots_to_make)) {
          cat(paste0("Working, item, ", aa, "/", n_plots_to_make, ", ", row.names(df_gs_anova)[aa], "\n"))
          if (aa %in% aa_bad) {next}
          ls_gratia[[aa]] <- try(gratia::draw(gs, select = aa , rug = FALSE))
        }## FOR ~ aa
        df_gs_anova[aa_bad, ]

      }## boo_qc


      # save to list
      ls_gratia <<- vector("list", group_n)

      # Create Plots
      for (a in group_n_seqlen) {
        plot_seq <<- df_plots_groups[df_plots_groups$plot_groups == a, "plot_n"]
        ls_gratia[[a]] <<- try(gratia::draw(gs, select = plot_seq , rug = FALSE))
        # # convert to plotly
        # if (format_rmd == "html") {
        #   ls_gratia[[a]] <- ggplotly(ls_gratia[[a]] )
        # }## IF ~ format_rmd
      }## FOR ~ a


      # Print Plots
      for (b in group_n_seqlen) {
        print(ls_gratia[[b]])
      }## FOR ~ b

    }## 6.0

    ### 7.0 contingency table station by depth----
    {

      # need a contingency table something like this ...figure we need >10 obs
      # to merit inclusion; needed to get rid of "odd" depths but certainly that's
      # a problem for other issues ... see XHH4931

      # should include stat infor: source station fixed cb_seg92 surrounding num_rec
      # sort in same order as above in section 4.0


      # *** JBH03.27: revised to simple rounding

      cross_tab <- dplyr::left_join(cb4d_data %>%
                               # select(station) %>%
                                 dplyr::group_by(station, in_subestuary, fixed, reg, cbseg_92) %>%
                                 dplyr::summarise(num_rec_tot = dplyr::n(), .groups = 'drop') ,
                             cb4d_data %>%
                               dplyr::mutate(depth_grp = round(depth, 0)) %>%
                               dplyr::group_by(station, depth_grp) %>%
                               dplyr::summarise(num_rec = dplyr::n(), .groups = 'drop') %>%  # varied depths
                               tidyr::pivot_wider(names_from = depth_grp
                                                  , values_from = num_rec
                                                  , values_fill = list(n = 0)
                                                  , names_sort = TRUE) ,
                             by = "station") %>%
        dplyr::arrange(dplyr::desc(in_subestuary),
                       dplyr::desc(fixed),
                       reg,
                       dplyr::desc(num_rec_tot),
                       station)

      tbl_07_01 <<- tblFT1(cross_tab,
                          tbl_title = "Observation count by station and depth",
                          tbl_theme = "box")
      tbl_07_01

    }## 7.0

    ### Excel ----
    ## Excel, Create ----
    # Create Excel Workbook and worksheets to export tables below
    sh_NOTES     <<- "NOTES"
    sh_tbl_01    <<- "model_info"
    sh_tbl_03_00 <<- "gam_formula"
    sh_tbl_03_01 <<- "gs_gamcoeff"
    sh_tbl_03_02 <<- "gs_anova"
    sh_tbl_04_01 <<- "stats"
    sh_tbl_07_01 <<- "cross_tab"

    xl_wb <<- openxlsx::createWorkbook()
    openxlsx::addWorksheet(xl_wb, sh_NOTES, tabColour = "darkgray")
    openxlsx::addWorksheet(xl_wb, sh_tbl_01)     # tbl_01
    openxlsx::addWorksheet(xl_wb, sh_tbl_03_00)  # tbl_03_00
    openxlsx::addWorksheet(xl_wb, sh_tbl_03_01)  # tbl_03_01
    openxlsx::addWorksheet(xl_wb, sh_tbl_03_02)  # tbl_03_02
    openxlsx::addWorksheet(xl_wb, sh_tbl_04_01)  # tbl_04_01
    openxlsx::addWorksheet(xl_wb, sh_tbl_07_01)  # tbl_07_01

    #### Excel, Data----
    # Add data to worksheets

    openxlsx::writeData(xl_wb
                        , sheet = sh_tbl_01
                        , x = data_01
                        , startCol = 1
                        , startRow = xl_SR
                        , headerStyle = style_bold
                        , withFilter = TRUE)
    # no filter
    openxlsx::writeData(xl_wb
                        , sheet = sh_tbl_03_00
                        , x = cb4d_model_summary_fmt$gam_formula
                        , startCol = 1
                        , startRow = xl_SR
                        , headerStyle = style_bold
                        , withFilter = FALSE)
    openxlsx::writeData(xl_wb
                        , sheet = sh_tbl_03_01
                        , x = gs_gamCoeff
                        , startCol = 1
                        , startRow = xl_SR
                        , headerStyle = style_bold
                        , withFilter = TRUE)
    openxlsx::writeData(xl_wb
                        , sheet = sh_tbl_03_02
                        , x = gs_ANOVA
                        , startCol = 1
                        , startRow = xl_SR
                        , headerStyle = style_bold
                        , withFilter = TRUE)
    openxlsx::writeData(xl_wb
                        , sheet = sh_tbl_04_01
                        , x = df_stats
                        , startCol = 1
                        , startRow = xl_SR
                        , headerStyle = style_bold
                        , withFilter = TRUE)
    openxlsx::writeData(xl_wb
                        , sheet = sh_tbl_07_01
                        , x = cross_tab
                        , startCol = 1
                        , startRow = xl_SR
                        , headerStyle = style_bold
                        , withFilter = TRUE)

    #### Excel, Formatting ----
    ##### Excel, Freeze Panes----
    openxlsx::freezePane(xl_wb
                         , sheet = sh_tbl_01
                         , firstActiveRow = xl_SR + 1)
    # don't need
    # openxlsx::freezePane(xl_wb
    #                      , sheet = sh_tbl_03_00
    #                      , firstActiveRow = xl_SR + 1)
    openxlsx::freezePane(xl_wb
                         , sheet = sh_tbl_03_01
                         , firstActiveRow = xl_SR + 1)
    openxlsx::freezePane(xl_wb
                         , sheet = sh_tbl_03_02
                         , firstActiveRow = xl_SR + 1)
    openxlsx::freezePane(xl_wb
                         , sheet = sh_tbl_04_01
                         , firstActiveRow = xl_SR + 1)
    openxlsx::freezePane(xl_wb
                         , sheet = sh_tbl_07_01
                         , firstActiveRow = xl_SR + 1)

    ##### Excel, WS Name to A1 ----
    # name
    openxlsx::writeData(xl_wb
                        , sheet = sh_tbl_01
                        , x = sh_tbl_01
                        , startCol = 1
                        , startRow = 1)
    openxlsx::writeData(xl_wb
                        , sheet = sh_tbl_03_00
                        , x = sh_tbl_03_00
                        , startCol = 1
                        , startRow = 1)
    openxlsx::writeData(xl_wb
                        , sheet = sh_tbl_03_01
                        , x = sh_tbl_03_01
                        , startCol = 1
                        , startRow = 1)
    openxlsx::writeData(xl_wb
                        , sheet = sh_tbl_03_02
                        , x = sh_tbl_03_02
                        , startCol = 1
                        , startRow = 1)
    openxlsx::writeData(xl_wb
                        , sheet = sh_tbl_04_01
                        , x = sh_tbl_04_01
                        , startCol = 1
                        , startRow = 1)
    openxlsx::writeData(xl_wb
                        , sheet = sh_tbl_07_01
                        , x = sh_tbl_07_01
                        , startCol = 1
                        , startRow = 1)
    # Style
    openxlsx::addStyle(xl_wb
                       , sheet = sh_tbl_01
                       , rows = 1
                       , cols = 1:4
                       , style = style_h1)
    openxlsx::addStyle(xl_wb
                       , sheet = sh_tbl_03_00
                       , rows = 1
                       , cols = 1:4
                       , style = style_h1)
    openxlsx::addStyle(xl_wb
                       , sheet = sh_tbl_03_01
                       , rows = 1
                       , cols = 1:4
                       , style = style_h1)
    openxlsx::addStyle(xl_wb
                       , sheet = sh_tbl_03_02
                       , rows = 1
                       , cols = 1:4
                       , style = style_h1)
    openxlsx::addStyle(xl_wb
                       , sheet = sh_tbl_04_01
                       , rows = 1
                       , cols = 1:4
                       , style = style_h1)
    openxlsx::addStyle(xl_wb
                       , sheet = sh_tbl_07_01
                       , rows = 1
                       , cols = 1:4
                       , style = style_h1)


    ##### Excel, Widths ----

    #### Excel, NOTES----

    win_user <<- Sys.getenv("USERNAME")

    notes_head <<- as.data.frame(cbind(c("CB4D Model Diagnostics"
                                        , "Report Data"
                                        , NA
                                        , "author@email.com"
                                        , as.character(Sys.Date())
                                        , NA
                                        , "Path & FileName"
                                        , "FileName"
                                        , "SheetName"
                                        , NA
                                        , "Description of Work"
                                        , ""
    )
    , c(rep(NA, 6)
        , '=LEFT(@CELL("filename",B7),FIND("]",@CELL("filename",B7)))'
        , '=MID(@CELL("filename",B8),FIND("[",@CELL("filename",B8)),(FIND("]",@CELL("filename",B8))-FIND("[",@CELL("filename",B8)))+1)'
        , '=MID(@CELL("filename",B9),FIND("]",@CELL("filename",B9))+1,LEN(@CELL("filename",B9))-FIND("]",@CELL("filename",B9)))'
        , rep(NA, 3))))
    #, c(rep(NA, 6), rep("formula", 3), rep(NA, 3))))
    class(notes_head[, 2]) <<- "formula"
    notes_toc <<- as.data.frame(rbind(
      c("NOTES", "Description of work and other worksheets", '=HYPERLINK(FileName&"NOTES"&"!A1","NOTES")')
      , c("model_info", "model parameters", '=HYPERLINK(FileName&"summary"&"!A1","summary")')
      , c("gs_gamcoef", "parametric coefficients", '=HYPERLINK(FileName&"topindicator"&"!A1","topindicator")')
      , c("gs_anova", "approximate significance of smooth terms", '=HYPERLINK(FileName&"samples"&"!A1","samples")')
      , c("stats", "summary statistics", '=HYPERLINK(FileName&"flags"&"!A1","flags")')
      , c("cross_tab", "contingency table", '=HYPERLINK(FileName&"site"&"!A1","site")')
    ))
    names(notes_toc) <<- c("Worksheet", "Description", "Link")
    class(notes_toc$Link) <<- "formula"


    openxlsx::writeData(xl_wb
                        , sheet = sh_NOTES
                        , x = notes_head
                        , startCol = 1
                        , startRow = 1
                        , colNames = FALSE)
    openxlsx::writeDataTable(xl_wb
                             , sheet = sh_NOTES
                             , x = notes_toc
                             , startCol = 1
                             , startRow = 15
                             , colNames = TRUE
                             , tableStyle = "TableStyleMedium9")

    openxlsx::addStyle(xl_wb
                       , sheet = sh_NOTES
                       , rows = 1
                       , cols = 1
                       , style = style_title)
    openxlsx::addStyle(xl_wb
                       , sheet = sh_NOTES
                       , rows = 2
                       , cols = 1
                       , style = style_h1)
    openxlsx::addStyle(xl_wb
                       , sheet = sh_NOTES
                       , rows = 4
                       , cols = 1
                       , style = style_hyperlink)
    openxlsx::addStyle(xl_wb
                       , sheet = sh_NOTES
                       , rows = 5
                       , cols = 1
                       , style = style_date)
    openxlsx::addStyle(xl_wb
                       , sheet = sh_NOTES
                       , rows = 7:9
                       , cols = 1
                       , style = style_bold)
    openxlsx::addStyle(xl_wb
                       , sheet = sh_NOTES
                       , rows = 11
                       , cols = 1
                       , style = style_h2)

    #### NOTES, Named Range
    openxlsx::createNamedRegion(xl_wb
                                , sheet = sh_NOTES
                                , name = "FileName"
                                , rows = 8
                                , cols = 2)

    #### Excel, Save----
    # same name as report
    fn_wb <<- file.path(dn_results
                       , paste0(fn_report_prefix
                                , tools::file_path_sans_ext(basename(i))
                                , "_Tables.xlsx")
    )
    openxlsx::saveWorkbook(xl_wb, fn_wb, overwrite = TRUE)

    # end Volume I



  } else if (report_version == 2) {
    # VOLUME 2: Base notebook----


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

    ## 1.0 meta information about model----
    {
      title_ith <- gsub("\\.rda$"
                        , ""
                        , paste(strsplit(basename(i), "~")[[1]], collapse = " | "))

      # *** JBH03.27: need to process dates and list in this tibble for output
      cb4d_model_summary_fmt <- cb4d_model_summary %>%
        dplyr::mutate(across(where(lubridate::is.POSIXct), ~format(.x, "%Y-%m-%d %H:%M:S %Z"))) %>%
        dplyr::mutate(across(where(is.list), ~paste(trans_parms, collapse = ", ")  ))
      data_01 <- tibble::tibble(FieldName = names(cb4d_model_summary_fmt)
                        , Value = as.vector(unlist(cb4d_model_summary_fmt)))
      tbl_01 <<- tblFT1(data_01
                       , tbl_title = title_ith
                       , tbl_theme = "box")
      tbl_01


    }## 1.0

    ## 2.0 model map----
    {

      # # *** JBH03.27: vastly revised functions
      # M2 <<- map_base() %>%
      #   map_polygon(sf_obj = cb4d_segments,
      #               obj_name = "cbseg_92",
      #               col_name = "in_subestuary",
      #               col_palette = c("goldenrod", "blue"),
      #               legend_title = "In Subestuary") %>%
      #   map_points(sf_obj = cb4d_stations,
      #              obj_name = "station",
      #              col_name = "fixed",
      #              col_palette = c("black", "red"),
      #              legend_title = "Fixed Station")
      # print(M2)

      # *** JBH 05.20: updated to address map pallette se
      # moved get_palette as a stand alone function

      # Get palettes for in_subestuary and fixed columns
      in_subestuary_palette <- get_palette(cb4d_segments$in_subestuary,
                                           "goldenrod",
                                           "blue")
      fixed_palette <- get_palette(cb4d_stations$fixed,
                                   "black",
                                   "red")

      # Create the map
      M2 <<- map_base() %>%
        map_polygon(sf_obj = cb4d_segments,
                    obj_name = "cbseg_92",
                    col_name = "in_subestuary",
                    col_palette = in_subestuary_palette,
                    legend_title = "In Subestuary") %>%
        map_points(sf_obj = cb4d_stations,
                   obj_name = "station",
                   col_name = "fixed",
                   col_palette = fixed_palette,
                   legend_title = "Fixed Station")

      print(M2)

      M2_static  <<- ggplot2::ggplot(cb4d_segments) +
        ggplot2::geom_sf(color = "blue") +
        ggplot2::geom_sf(data = cb4d_stations, color = "red") +
        ggplot2::labs(title = cb4d_model_summary$subestuary)

      # print(M2_static)

    }## 2.0

    ## 3.0 model summary----
    {
      # substantively want what we get from
      # summary(gs)

      # *** JBH03.27: added tbl_03_00
      tbl_03_00 <<- tblFT1(tibble::tibble(gam_formula = cb4d_model_summary_fmt$gam_formula))
      tbl_03_00

      gs_gamCoeff <- gamCoeff(gs)
      tbl_03_01 <<- tblFT1(gs_gamCoeff, tbl_title = "Parametric Coefficients")
      tbl_03_01

      gs_ANOVA <- gamANOVA(gs)
      tbl_03_02 <<- tblFT1(gs_ANOVA, tbl_title = "Approximate significance of smooth terms:")
      tbl_03_02

    }## 3.0

    ## 4.0 statistics by station and 5.0 NEW----
    {

      station <- strsplit(basename(i), "~")[[1]][2]

      station_vec <- dplyr::filter(cb4d_data, in_subestuary == TRUE) %>%
        dplyr::select(station) %>%
        unique() %>%
        unlist(use.names = FALSE)

      # Borrow depths table (7.0) from Volume 1 report
      cross_tab <- dplyr::left_join(cb4d_data %>%
                               # select(station) %>%
                                 dplyr::group_by(station, in_subestuary,
                                                 fixed,
                                                 reg,
                                                 cbseg_92) %>%
                                 dplyr::summarise(num_rec_tot = dplyr::n(),
                                                  .groups = 'drop') ,
                             cb4d_data %>%
                               dplyr::mutate(depth_grp = round(depth, 0)) %>%
                               dplyr::group_by(station, depth_grp) %>%
                               dplyr::summarise(num_rec = dplyr::n(),
                                                .groups = 'drop') %>%  # varied depths
                               tidyr::pivot_wider(names_from = depth_grp
                                           , values_from = num_rec
                                           , values_fill = list(n = 0)
                                           , names_sort = TRUE) ,
                             by = "station") %>%
        dplyr::arrange(dplyr::desc(in_subestuary),
                       dplyr::desc(fixed),
                       reg,
                       dplyr::desc(num_rec_tot),
                       station)

      tbl_cross_tab <<- tblFT1(cross_tab
                              , tbl_title = "Observation count by station and depth"
                              , tbl_theme = "box")
      tbl_cross_tab

      # desired depths
      depth_target <<- c(1, 6, 12, 18, 24, 30)
      # surface, above picnocline, below picnocline, bottom, bottom, bottom)

      len_station_vec <- length(station_vec)
      seq_len_station_vec <<- seq_len(len_station_vec)
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


      ### Loop ----
      for (s in seq_len_station_vec) {
        # s<-1
        cat(paste0(s,"\n"))
        s_name <- station_vec[s]

        ### depth vector ----
        cross_tab_s <- dplyr::filter(cross_tab, station == s_name) %>%
          dplyr::select_if(~ !any(is.na(.)))

        s_depth <- as.numeric(names(cross_tab_s)[!names(cross_tab_s) %in% col_drop])

        s_depth_n <- dplyr::select(cross_tab_s, as.character(s_depth)) %>%
          unlist(use.names = FALSE)

        s_depth_n_total <- dplyr::select(cross_tab_s, num_rec_tot) %>%
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


        ### 4, plot ----
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

        ls_faceted <<- ls_faceted
        ### 5, prediction ----

        #~~~~~~~~~~~   make function: make_prediction_data_set
        # first make prediction data set

        p_dat_01 <<- cb4d_stations %>%
          dplyr::select(station, depth_b, wb_lat_km, wb_lon_km, reg) %>%
          dplyr::filter(station %in% s_name) %>%
          sf::st_set_geometry(NULL)

        p_dat_02 <<- tibble::tibble(depth = depth_target_s)

        p_dat_03 <<- generate_date_time(start_date_time ,
                                       stop_date_time ,
                                       time_step , time_units = "days", tz = "EST") %>%
          dplyr::mutate(doy = leap_yday(date_time),
                 date_d = lubridate::year(date_time) + leap_yday(date_time)/366)

        p_dat <<- tidyr::crossing(p_dat_01, p_dat_02, p_dat_03)

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
          dplyr::mutate(depth_grp = round(depth, 0)) %>%
          dplyr::filter(station %in% s_name,
                        depth_grp %in% depth_target_s) %>%
          dplyr::mutate(station_depth = paste0("Station: ",
                                               station,
                                               ", Depth: ",
                                               depth_grp,
                                               " m"))


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
          dplyr::filter(station %in% s_name,
                        depth %in% depth_target_s) %>%
          dplyr::mutate(station_depth = paste0("Station: ",
                                               station,
                                               ", Depth: ",
                                               depth,
                                               " m"))

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



        ls_p5[[s]] <- try(ggplot2::ggplot(data, aes(x = date_time, y = y_obs)) +
                             ggplot2::geom_point(color = "red",
                                                 shape = 1,
                                                 alpha = 1) +
                             ggplot2::geom_line(data = pred,
                                                aes(x = date_time, y = y_pred),
                                                color = "black") +
                             ggplot2::facet_wrap(~station_depth,
                                                 scales = "free_x") +
                             ggplot2::theme_minimal() +
                             ggplot2::theme(panel.border = element_rect(color = "black"
                                                              , fill = NA
                                                              , linewidth = 0.25)
                                  , strip.text = element_text(size = 8)) +
                             ggplot2::xlab("Date") +
                             ggplot2::ylab("") )

        ls_p5_dat[[s]] <- pred

      }## FOR ~ s

      ls_p5 <<- ls_p5

      # write xlsx for p5 data
      fn_wb <<- file.path(dn_results
                         , paste0(fn_report_prefix
                                  , tools::file_path_sans_ext(basename(i))
                                  , "_Predictions.xlsx")
      )
      openxlsx::write.xlsx(ls_p5_dat, fn_wb)


    }## 4.0 and 5.0

    # end Volume II








  } else {
    msg <- paste0("Invalid report version specified.\n",
                  "Accepted values are 1 or 2.")
    stop(msg)
  }## IF ~ report_version


}## FUNCTION

