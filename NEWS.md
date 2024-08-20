NEWS
================
<Erik.Leppo@tetratech.com> and <jon.harcum@tetratech.com>

<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->

    #> Last Update: 2024-08-20 14:07:08.207255

# tidBIT 0.1.0.9023

- refactor correlation and covariance functions

# tidBIT 0.1.0.9022

- refactor correlation and covariance functions

# tidBIT 0.1.0.9021

- added fivenum_char, fivenum_charp, …

# tidBIT 0.1.0.9020

- added plot_mat_cov (Matrix-Based Covariance Plot)

# tidBIT 0.1.0.9019

- added plot_matr_corr (Matrix-Based Correlation Plot)

# tidBIT 0.1.0.9018

- ar1 modeling refactored, migrate from as.Date to lubridate::date

# tidBIT 0.1.0.9017

- leap_yday added to package help page; supporting leap_yday functions
  refactored

# tidBIT 0.1.0.9016

- refactored to add helper functions decimal_date and decimal_hours

# tidBIT 0.1.0.9015

- refactored leap_yday to allow for arbritrary start day

# tidBIT 0.1.0.9014

- refactored leap_yday to allow for ‘water’- or ‘climate’- based year

# tidBIT 0.1.0.9013

- ar(1) function documentation improved

# tidBIT 0.1.0.9012

- functions arima_def, arima_alt, arima_mm, estimate_ar1, plot_acf_pacf
  refactored to improve documentation and remove options for user to try
  and use order != c(1,0,0)

# tidBIT 0.1.0.9011

- functions arima_def, arima_alt, arima_mm, estimate_ar1, plot_acf_pacf
  added

# tidBIT 0.1.0.9010

- functions plot_obs_pred_res, plot_bivariate_data, and
  plot_4_panel_distr added

# tidBIT 0.1.0.9008

- refactor: reduced exported functions

# tidBIT 0.1.0.9007

- refactor: Corrected beta logit documentation, Issue \#3
  - fun_beta_logit.R

# tidBIT 0.1.0.9006

- refactor: Corrected beta logit transformation, Issue \#2
  - fun_beta_logit.R

# tidBIT 0.1.0.9005

- feature: Add new functions and checked with Check Package
  - fun_cb4d_buildout_01.R

# tidBIT 0.1.0.9004

- refactor: Update NEWS install section and format, Issue \#1

# tidBIT 0.1.0.9003

- docs: Update package documentation
  - DESCRIPTION
  - NEWS
  - README
  - License (MIT)
- functions: Add new function files
  - fun_beta_logit.R
  - fun_cb4d_buildout_00.R
  - fun_mapping.R
  - tidBIT.R
- style: Add style to functions
  - Wrap text at 80 lines
  - Denote ending braces for functions
- documentation: Run CMD check and fix issues
  - Add packages to DESCRIPTION
  - Add bindings for global variables for functions
  - Add missing ImportFrom functions

# tidBIT 0.1.0.9002

- docs: Add issue templates to repository

# tidBIT 0.1.0.9001

- Create Repository on GitHub
