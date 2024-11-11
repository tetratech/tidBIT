
# VOLUME 1: Base notebook
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

# suppressPackageStartupMessages(library(conflicted))     # new
# suppressPackageStartupMessages(library(leaflet.extras)) # new
# suppressPackageStartupMessages(library(htmlwidgets))    # new
# suppressPackageStartupMessages(library(htmltools))      # new
# suppressPackageStartupMessages(library(viridis))        # new
# suppressPackageStartupMessages(library(writexl))        # new
# suppressPackageStartupMessages(library(pander))         # new
# suppressPackageStartupMessages(library(gratia))         # new
# suppressPackageStartupMessages(library(rlang))          # DESCRIPTION
# suppressPackageStartupMessages(library(tools))          # new
# suppressPackageStartupMessages(library(gridExtra))      # new
# suppressPackageStartupMessages(library(hexbin))         # new
# suppressPackageStartupMessages(library(plotly))         # new
# library(openxlsx)                                       # DESCRIPTION
conflicts_prefer(dplyr::filter, .quiet = TRUE)
conflicts_prefer(readxl::read_xlsx, .quiet = TRUE)
conflicts_prefer(dplyr::select, .quiet = TRUE)
conflicts_prefer(dplyr::lag, .quiet = TRUE)

# > flextable settings ####
flextable::set_flextable_defaults(
  big.mark = "",
  font.size = 10, font.family = "Calibri",
  font.color = "#333333",
  table.layout =   "autofit",
  border.color = "gray",
  padding.top = 0, padding.bottom = 0,
  padding.left = 2, padding.right = 2)

# source("diagnostics_scripts/settings_Excel.R")
## make a new function

# source("diagnostics_scripts/fun_beta_logit.R")
# ok, run_$_transformations.R


# source("diagnostics_scripts/fun_cb4d_buildout_00.R")
# ok, initialization

# source("diagnostics_scripts/fun_cb4d_buildout_01.R")
# new??

# source("diagnostics_scripts/fun_cb4d_diagnostics_00.R")
## Mostly general plotting, missing 2nd havel after plot4panel

# source("diagnostics_scripts/fun_mapping.R")
# ok fun_mapping.R

# source("diagnostics_scripts/fun_mapping_tools.R")
# copied over





# # *** JBH03.27: commented out this section to simplify
# # and use "i"
#
# # Load Model----
# ## Load Method ----
# ## Loads with variable set in report_diagnostic()
# ### else loads user files
# if (run_type == "script") {
#   # Load RDA via report_diagnostic() function
#   # **DO NOT** EDIT this part of the IF statement!
#   # i defined in calling function
#   # Remove some left over objects if this is not the first segment
#  # if (exists("cb4d_segments_sf")) {rm(cb4d_segments_sf)}
# } else {
#   # run_type == "interactive"
#   # Load RDA (model run): file name
#   gs_file <- "do~CB3~datahub~2016-2018~2023_10_07_005~by_term2~none.rda"
#   # Load RDA (model run): tcltk
#   # path_model_rda  <- tcltk::tk_choose.files()
#   path_model_rda <- file.path("inst", "extdata", gs_file)
#   dn_model_rda    <- dirname(path_model_rda)
#   fn_model_rda    <- basename(path_model_rda)
#   gs_file <- fn_model_rda # using tcltk from run_reports.R
#   #fn_rmd <- paste0("report_vol1~", tools::file_path_sans_ext(gs_file))
#   # dn_models <- file.path("cb4d_model", "cb4d_gam_models")
#   # gs_path <- file.path(dn_models, gs_file)
#   # gs_path <- file.choose()
#   # i <- gs_path
#   i <- path_model_rda
# }## IF ~ run_type

## Load ----

# i<- "cb4d_model/cb4d_gam_models/do~CB3~all~2016-2018~2023_10_07_005~by_term2~BL_010_060.rda"
load(i)


# *** JBH03.27: revised/combined "add on" variables

## data ----
pry(gs, c("cb4d_data"
          , "cb4d_model_summary"
          , "cb4d_segments"
          , "cb4d_stations"))

# names(gs)
# "cb4d_data"          <- data used in model
# "cb4d_model_summary" <- summary "one-liner" statistics
# "cb4d_stations"      <- station list, lat/lng, fixed vs. other, would need to join with cb4d_segments to figure out Primary vs. surrounding
# "cb4d_segments"      <- list of segments in model, differentiates Primary vs. surrounding

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
  dplyr::mutate(dplyr::across(dplyr::where(lubridate::is.POSIXct), ~format(.x, "%Y-%m-%d %H:%M:S %Z"))) %>%
  dplyr::mutate(dplyr::across(dplyr::where(is.list), ~paste(trans_parms, collapse = ", ") ))
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
  M1 <- map_base() %>%
    map_polygon(sf_obj = cb4d_segments, obj_name = "cbseg_92",
                col_name = "in_subestuary", col_palette = c("goldenrod", "blue"),
                legend_title = "In Subestuary") %>%
    map_points(sf_obj = cb4d_stations, obj_name = "station",
               col_name = "fixed", col_palette = c("black", "red"),
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
  M1_static  <- ggplot2::ggplot(cb4d_segments) +
    ggplot2::geom_sf(color = "blue") +
    ggplot2::geom_sf(data = cb4d_stations, color = "red") +
    ggplot2::labs(title = cb4d_model_summary$subestuary)

  print(M1_static)

}## 2.0

# 3.0 model summary----
{
  # substantively want what we get from
  # summary(gs)

  # *** JBH03.27: added tbl_03_00
  tbl_03_00 <- tblFT1(tibble(gam_formula = cb4d_model_summary_fmt$gam_formula))
  tbl_03_00

  gs_gamCoeff <- gamCoeff(gs)
  tbl_03_01 <- tblFT1(gs_gamCoeff
                      , tbl_title = "Parametric Coefficients")
  tbl_03_01

  gs_ANOVA <- gamANOVA(gs)
  tbl_03_02 <- tblFT1(gs_ANOVA
                      , tbl_title = "Approximate significance of smooth terms:")
  tbl_03_02

}## 3.0

# 4.0 statistics by station----
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
    group_by(station, in_subestuary, fixed, reg, cbseg_92) %>%
    summarize(num_rec = n()
              , sse = sum((y_obs - y_pred)^2)
              , sst = sum((y_obs - mean(y_obs))^2)
              , rmse = sqrt(mean((y_obs - y_pred)^2))
              , r_sq = 1 - (sse / sst)
              , .groups = "drop_last") %>%
    arrange(desc(in_subestuary),  desc(fixed), reg, desc(num_rec), station)
  tbl_04_01 <- tblFT1(df_stats
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



# 5.0 simple diagnostic plots (these should be ok for now)----
{
  # unable to convert either to plotly (some features not enabled)

  plot_05_01 <- plot_4_panel_distr(cb4d_data$y_obs, vlab = "Observed", main = title_ith, plot_type = "thin")
  # plot_4_panel_distr(cb4d_data$y_obs, vlab = "Observed", main = title_ith)
  plot_05_01


  plot_05_02 <- plot_obs_pred_res(cb4d_data, main = title_ith, plot_type = "thin", smooth_line=TRUE)
  # plot_obs_pred_res(cb4d_data, main = title_ith, plot_type = "hex", smooth_line=TRUE)
  # plot_obs_pred_res(cb4d_data, main = title_ith, smooth_line=TRUE)
  plot_05_02

  # enhancements (plots by "in_subestuary"):
  # plot_obs_pred_res(cb4d_data[cb4d_data$in_subestuary,], main = title_ith, plot_type = "all", smooth_line=TRUE)
  # plot_obs_pred_res(cb4d_data[cb4d_data$in_subestuary & cb4d_data$fixed,], main = title_ith, plot_type = "all", smooth_line=TRUE)

}## 5.0


# 6.0 gratia::draw----
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
  group_n_seqlen <- seq_len(group_n)
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
  ls_gratia <- vector("list", group_n)

  # Create Plots
  for (a in group_n_seqlen) {
    plot_seq <- df_plots_groups[df_plots_groups$plot_groups == a, "plot_n"]
    ls_gratia[[a]] <- try(gratia::draw(gs, select = plot_seq , rug = FALSE))
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

# 7.0 contingency table station by depth----
{

  # need a contingency table something like this ...figure we need >10 obs
  # to merit inclusion; needed to get rid of "odd" depths but certainly that's
  # a problem for other issues ... see XHH4931

  # should include stat infor: source station fixed cb_seg92 surrounding num_rec
  # sort in same order as above in section 4.0


  # *** JBH03.27: revised to simple rounding

  cross_tab <- left_join(cb4d_data %>%
                           # select(station) %>%
                           group_by(station, in_subestuary, fixed, reg, cbseg_92) %>%
                           summarise(num_rec_tot = n(), .groups = 'drop') ,
                         cb4d_data %>%
                           mutate(depth_grp = round(depth, 0)) %>%
                           group_by(station, depth_grp) %>%
                           summarise(num_rec = n(), .groups = 'drop') %>%  # varied depths
                           tidyr::pivot_wider(names_from = depth_grp
                                       , values_from = num_rec
                                       , values_fill = list(n = 0)
                                       , names_sort = TRUE) ,
                         by = "station") %>%
    arrange(desc(in_subestuary), desc(fixed), reg, desc(num_rec_tot), station)

  tbl_07_01 <- tblFT1(cross_tab, tbl_title = "Observation count by station and depth", tbl_theme = "box")
  tbl_07_01

}## 7.0

# Excel ----
## Excel, Create ----
# Create Excel Workbook and worksheets to export tables below
sh_NOTES     <- "NOTES"
sh_tbl_01    <- "model_info"
sh_tbl_03_00 <- "gam_formula"
sh_tbl_03_01 <- "gs_gamcoeff"
sh_tbl_03_02 <- "gs_anova"
sh_tbl_04_01 <- "stats"
sh_tbl_07_01 <- "cross_tab"

xl_wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(xl_wb, sh_NOTES, tabColour = "darkgray")
openxlsx::addWorksheet(xl_wb, sh_tbl_01)     # tbl_01
openxlsx::addWorksheet(xl_wb, sh_tbl_03_00)  # tbl_03_00
openxlsx::addWorksheet(xl_wb, sh_tbl_03_01)  # tbl_03_01
openxlsx::addWorksheet(xl_wb, sh_tbl_03_02)  # tbl_03_02
openxlsx::addWorksheet(xl_wb, sh_tbl_04_01)  # tbl_04_01
openxlsx::addWorksheet(xl_wb, sh_tbl_07_01)  # tbl_07_01

## Excel, Data----
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

## Excel, Formatting ----
### Excel, Freeze Panes----
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

### Excel, WS Name to A1 ----
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


### Excel, Widths ----

## Excel, NOTES----

win_user <- Sys.getenv("USERNAME")

notes_head <- as.data.frame(cbind(c("Project Name"
                                    , "Specific Task"
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
class(notes_head[, 2]) <- "formula"
notes_toc <- as.data.frame(rbind(
  c("NOTES", "Description of work and other worksheets", '=HYPERLINK(FileName&"NOTES"&"!A1","NOTES")')
  , c("model_info", "model parameters", '=HYPERLINK(FileName&"summary"&"!A1","summary")')
  , c("gs_gamcoef", "parametric coefficients", '=HYPERLINK(FileName&"topindicator"&"!A1","topindicator")')
  , c("gs_anova", "approximate significance of smooth terms", '=HYPERLINK(FileName&"samples"&"!A1","samples")')
  , c("stats", "summary statistics", '=HYPERLINK(FileName&"flags"&"!A1","flags")')
  , c("cross_tab", "contingency table", '=HYPERLINK(FileName&"site"&"!A1","site")')
))
names(notes_toc) <- c("Worksheet", "Description", "Link")
class(notes_toc$Link) <- "formula"


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

## Excel, Save----
# same name as report
fn_wb <- file.path(dn_results
                   , paste0(fn_report_prefix
                            , tools::file_path_sans_ext(basename(i))
                            , "_Tables.xlsx")
)
openxlsx::saveWorkbook(xl_wb, fn_wb, overwrite = TRUE)

# end Volume I
