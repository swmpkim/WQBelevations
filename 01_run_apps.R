# Use this file to run either shiny app.
# Only run one of the provided lines of code,
# depending on which app you want to use.

# Vegetation + Elevations
shiny::runApp(here::here("app_files",
                         "app_vegAndElevation.R"))


# Vegetation only
shiny::runApp(here::here("app_files",
                         "app_vegOnly.R"))
