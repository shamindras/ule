# README
# 1. This is a one-off script to initially setup up renv for this project
# 2. It should only be run once to install the required initial packages when
#    setting up the github repo for this project.

# Install {renv} ----
# install.packages("renv")

# Initialize a bare {renv} for the project ----
# renv::init(bare = TRUE)

# Install {pak} using {renv} ----
# renv::install("pak")

# Define the required packages ----
PKGS <- c(
  "assertthat",
  "cowplot",
  "gganimate",
  "glue",
  "here",
  "isotone",
  "latex2exp",
  "magrittr",
  "milesmcbain/fnmate",
  "patchwork",
  "purrr",
  "sf",
  "styler",
  "targets",
  "transformr",
  "uniformly",
  "usethis",
  "tidyverse",
  "tikzDevice"
)

# {pak} install required packages
pak::pkg_install(PKGS)

# Take snapshot of {renv}
# renv::snapshot()
