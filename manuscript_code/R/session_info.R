# Record the R session used by the analysis: loads every package the pipeline uses and writes
# outputs/logs/sessionInfo.txt. Called at the end of run_all.R.
pkgs <- c("dplyr", "tidyr", "readr", "lubridate", "stringr", "purrr", "zoo", "fuzzyjoin", "goFlux",
          "mgcv", "lme4", "epiR", "ineq", "ggplot2", "patchwork", "gridExtra", "ggridges", "ragg",
          "sf", "maptiles", "tidyterra", "ggspatial", "ggrepel")
suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
writeLines(capture.output(sessionInfo()), file.path("outputs", "logs", "sessionInfo.txt"))
