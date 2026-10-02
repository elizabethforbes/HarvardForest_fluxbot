# Step 1b. Fluxbot fluxes with the originally submitted fit window (56:00-60:00) for the fit-window
# sensitivity analysis (2_analysis/12_window_sensitivity.R, Fig. S2). Same procedure as
# 01_fluxes.R; writes data_clean/fluxes/fluxbot_fluxes_w56.csv.
Sys.setenv(FB_WINDOW_START = "56")
source("1_clean/01_fluxes.R")
