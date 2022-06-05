PKGS <- c(
  "assertthat",
  "dplyr",
  "gganimate",
  "ggplot2",
  "glue",
  "isotone",
  "magrittr",
  "milesmcbain/fnmate",
  "patchwork",
  "purrr",
  "sf",
  "styler",
  "targets",
  "tibble",
  "tidyr",
  "transformr",
  "uniformly",
  "usethis"
)

# renv::install("pak")
pak::pkg_install(PKGS)
